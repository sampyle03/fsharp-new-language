/*
* PlotWindow.xaml.cs
* ------------------
* This file is responsible for drawing function plots in the Numera GUI.
*
* The plot window receives a list of (x, y) points from the F# interpreter
* (generated from a command such as: graph y = x^2, (-5, 5);),
* and then:
*  - scales the axes to fit the data,
*  - draws grid lines, axes, tick marks and labels,
*  - and renders the function curve on a Canvas.
*
* Zoom/pan:
* The plot uses a simple "camera" model (centre + scale). Panning moves the camera centre.
* Zooming changes the camera scale, and we zoom around the mouse pointer so it feels natural.
* When the user pans/zooms, we re-request fresh points from the F# backend for the currently
* visible x-range. This makes the curve behave like it continues indefinitely.
*
* Smooth curve fitting (spline interpolation):
* The curve is rendered using cubic spline interpolation based on the approach
* by Scott W. Harden (Jan 22, 2022), which adapts original work by Ryan Seghers. See:
* https://swharden.com/blog/2022-01-22-spline-interpolation/
*/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

namespace NumeraGUI
{
    /// <summary>
    /// PlotWindow is the WPF window responsible for plotting functions onto a Canvas.
    /// It stores the plotted points in "world" coordinates, and uses a camera model
    /// (centre + scale) to support pan and zoom.
    /// </summary>
    public partial class PlotWindow : Window
    {
        // ===================== Layout & Camera Constants =====================

        // Padding around the plot area so axis labels do not overlap the border.
        private const double PlotPaddingLeft = 50;
        private const double PlotPaddingRight = 20;
        private const double PlotPaddingTop = 20;
        private const double PlotPaddingBottom = 40;

        // Zoom limits for the camera view.
        private const double MinZoom = 0.2;
        private const double MaxZoom = 20.0;
        private const double ZoomStep = 1.10; // ~10% per wheel notch

        // ===================== Plot Data & Backend Sampling =====================

        // Stores the current set of plotted points (world coordinates).
        private readonly List<Point> _points = new List<Point>();

        // Remembers the last "graph y = expr" part (without the range),
        // so we can re-request points when the user pans/zooms.
        private string _baseGraphCommandPrefix = null;

        // Debounce timer so we do not call the F# backend on every single mouse move while panning.
        private System.Windows.Threading.DispatcherTimer _resampleTimer;

        // Delegate set by MainWindow: given a graph command string, returns points from F#.
        public Func<string, IEnumerable<Point>> GetPointsForExpression { get; set; }

        // ===================== World / Fit / Camera State =====================

        // World-coordinate bounds representing the CURRENT visible window.
        // These update dynamically as the user pans/zooms.
        private double _worldMinX = -10.0;
        private double _worldMaxX = 10.0;
        private double _worldMinY = -10.0;
        private double _worldMaxY = 10.0;

        // "Fit" bounds are computed from the data (with padding) whenever a new plot is loaded.
        // Zoom scaling is relative to this fitted window (scale=1.0 is the fitted view).
        private double _fitMinX = -10.0;
        private double _fitMaxX = 10.0;
        private double _fitMinY = -10.0;
        private double _fitMaxY = 10.0;

        // Camera/view state in world coordinates.
        private double _viewCenterX = 0.0;
        private double _viewCenterY = 0.0;
        private double _viewScale = 1.0;

        // ===================== Pan Interaction State =====================

        private bool _isPanning = false;
        private Point _panStartMouse;
        private double _panStartCenterX;
        private double _panStartCenterY;

        // ===================== Window Setup =====================

        public PlotWindow()
        {
            InitializeComponent();

            // Redraw the plot whenever the window is resized.
            if (PlotCanvas != null)
            {
                PlotCanvas.SizeChanged += (s, e) => DrawPlot();
            }

            // Timer used to debounce resampling while panning.
            // This keeps dragging smooth, and then refreshes the data once the user pauses.
            _resampleTimer = new System.Windows.Threading.DispatcherTimer();
            _resampleTimer.Interval = TimeSpan.FromMilliseconds(150);
            _resampleTimer.Tick += (s, e) =>
            {
                _resampleTimer.Stop();
                ResamplePointsForCurrentView();
            };
        }

        private void PlotWindow_Loaded(object sender, RoutedEventArgs e)
        {
            DrawPlot();
        }

        // ===================== Small Utilities =====================

        /// <summary>
        /// Clamp a value between a minimum and maximum.
        /// </summary>
        private static double Clamp(double v, double min, double max)
        {
            if (v < min) return min;
            if (v > max) return max;
            return v;
        }

        /// <summary>
        /// Extracts the "graph y = expr" prefix from whatever the user typed.
        /// We store this so pan/zoom can rebuild the command with a new range.
        /// </summary>
        private static string ExtractCommandPrefix(string input)
        {
            if (string.IsNullOrWhiteSpace(input))
                return null;

            string trimmed = input.Trim().TrimEnd(';');

            int commaIndex = trimmed.IndexOf(',');
            if (commaIndex >= 0)
                return trimmed.Substring(0, commaIndex).Trim(); // "graph y = x^2"
            else
                return trimmed.Trim(); // user didn't type a range
        }

        /// <summary>
        /// Builds a full graph command string that the F# parser already understands.
        /// We use invariant formatting so decimals always parse reliably.
        /// </summary>
        private static string BuildGraphCommand(string prefix, double xmin, double xmax, double dx)
        {
            string f(double v) => v.ToString("G17", System.Globalization.CultureInfo.InvariantCulture);
            return $"{prefix}, ({f(xmin)}, {f(xmax)}, {f(dx)});";
        }

        // ===================== Fit / Camera Helpers =====================

        /// <summary>
        /// Reset the camera back to the fitted view (centered and scale 1.0).
        /// </summary>
        private void ResetViewToFit()
        {
            _viewScale = 1.0;
            _viewCenterX = (_fitMinX + _fitMaxX) / 2.0;
            _viewCenterY = (_fitMinY + _fitMaxY) / 2.0;

            ApplyViewToWorldBounds();
        }

        /// <summary>
        /// Updates _worldMin/_worldMax from the current camera state.
        /// Visible span is the fitted span divided by zoom scale.
        /// </summary>
        private void ApplyViewToWorldBounds()
        {
            double fitSpanX = _fitMaxX - _fitMinX;
            double fitSpanY = _fitMaxY - _fitMinY;

            if (fitSpanX <= 0 || fitSpanY <= 0)
            {
                _worldMinX = -10.0;
                _worldMaxX = 10.0;
                _worldMinY = -10.0;
                _worldMaxY = 10.0;
                return;
            }

            double visibleSpanX = fitSpanX / _viewScale;
            double visibleSpanY = fitSpanY / _viewScale;

            _worldMinX = _viewCenterX - visibleSpanX / 2.0;
            _worldMaxX = _viewCenterX + visibleSpanX / 2.0;
            _worldMinY = _viewCenterY - visibleSpanY / 2.0;
            _worldMaxY = _viewCenterY + visibleSpanY / 2.0;
        }

        /// <summary>
        /// Convert a canvas pixel position to a world-coordinate position
        /// using the current world bounds.
        /// </summary>
        private Point ScreenToWorld(Point canvasPt)
        {
            double width = PlotCanvas.ActualWidth;
            double height = PlotCanvas.ActualHeight;

            double usableWidth = width - PlotPaddingLeft - PlotPaddingRight;
            double usableHeight = height - PlotPaddingTop - PlotPaddingBottom;

            double rangeX = _worldMaxX - _worldMinX;
            double rangeY = _worldMaxY - _worldMinY;

            if (usableWidth <= 0 || usableHeight <= 0 || rangeX <= 0 || rangeY <= 0)
                return new Point(_viewCenterX, _viewCenterY);

            double x = _worldMinX + ((canvasPt.X - PlotPaddingLeft) / usableWidth) * rangeX;
            double y = _worldMinY + ((usableHeight - (canvasPt.Y - PlotPaddingTop)) / usableHeight) * rangeY;

            return new Point(x, y);
        }

        // ===================== Option A: Resample Points For Visible Window =====================

        /// <summary>
        /// Picks a step size based on the current visible x-span and canvas width.
        /// The idea is roughly "one point per pixel", clamped so it stays responsive.
        /// </summary>
        private double ComputeDxForCurrentView(double xmin, double xmax)
        {
            double span = xmax - xmin;
            if (span <= 0)
                return 0.1;

            double width = PlotCanvas.ActualWidth;
            double usableWidth = width - PlotPaddingLeft - PlotPaddingRight;

            // Fallback if the window is not measured yet.
            if (usableWidth <= 50)
                usableWidth = 800;

            int targetPoints = (int)Math.Round(usableWidth);

            // Clamp so we do not hammer the backend for huge ranges,
            // but still keep it smooth for normal use.
            targetPoints = (int)Clamp(targetPoints, 400, 2500);

            // Stay comfortably under the F# safety cap (5000).
            targetPoints = Math.Min(targetPoints, 4500);

            return span / (targetPoints - 1);
        }

        /// <summary>
        /// Requests a fresh set of points from the F# backend using the currently visible x-range.
        /// This is what makes the curve feel like it continues "forever" when you pan/zoom.
        /// </summary>
        private void ResamplePointsForCurrentView()
        {
            if (GetPointsForExpression == null || string.IsNullOrWhiteSpace(_baseGraphCommandPrefix))
                return;

            if (PlotCanvas == null)
                return;

            double xmin = _worldMinX;
            double xmax = _worldMaxX;

            if (xmax <= xmin)
                return;

            double dx = ComputeDxForCurrentView(xmin, xmax);
            string cmd = BuildGraphCommand(_baseGraphCommandPrefix, xmin, xmax, dx);

            try
            {
                var newPoints = GetPointsForExpression(cmd);

                // IMPORTANT:
                // Replace the points without refitting the view, otherwise the user's pan/zoom gets undone.
                _points.Clear();
                if (newPoints != null)
                    _points.AddRange(newPoints);

                DrawPlot();
            }
            catch
            {
                // If the backend errors while panning into awkward ranges, we just keep the last valid plot.
            }
        }

        // ===================== UI Buttons =====================

        /// <summary>
        /// Handles the Plot button.
        /// Reads the expression from the text box, asks F# to generate points,
        /// then fits the view and draws the plot.
        /// </summary>
        private void PlotButton_Click(object sender, RoutedEventArgs e)
        {
            if (GetPointsForExpression == null)
                return;

            string expr = FunctionTextBox.Text;

            if (string.IsNullOrWhiteSpace(expr))
                return;

            // Save the command prefix so pan/zoom can rebuild the command with a new range.
            _baseGraphCommandPrefix = ExtractCommandPrefix(expr);

            try
            {
                var newPoints = GetPointsForExpression(expr);
                PlotFromCoordinates(newPoints);
            }
            catch (Exception ex)
            {
                MessageBox.Show(
                    $"Error while plotting:\n{ex.Message}",
                    "Plot Error",
                    MessageBoxButton.OK,
                    MessageBoxImage.Error);
            }
        }

        /// <summary>
        /// Displays a short help message explaining the expected graph syntax.
        /// </summary>
        private void HelpButton_Click(object sender, RoutedEventArgs e)
        {
            var plotHelpWindow = new PlotHelpWindow
            {
                Owner = this
            };

            plotHelpWindow.ShowDialog();
        }

        /// <summary>
        /// Clears the current plot and resets the input box.
        /// This does not change any interpreter variables - it only clears the visual plot.
        /// </summary>
        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            _points.Clear();
            PlotCanvas.Children.Clear();
            FunctionTextBox.Clear();

            _baseGraphCommandPrefix = null;

            // Reset to defaults
            _fitMinX = -10.0;
            _fitMaxX = 10.0;
            _fitMinY = -10.0;
            _fitMaxY = 10.0;

            ResetViewToFit();
            DrawPlot();
        }

        // ===================== Camera Controls: Zoom & Pan =====================

        /// <summary>
        /// Mouse wheel zooms in/out around the mouse pointer position.
        /// We also resample points immediately so the curve stays smooth at the new zoom level.
        /// </summary>
        private void PlotCanvas_MouseWheel(object sender, MouseWheelEventArgs e)
        {
            if (PlotCanvas == null || _points.Count == 0)
                return;

            // World position under mouse BEFORE zoom
            Point mouse = e.GetPosition(PlotCanvas);
            Point worldBefore = ScreenToWorld(mouse);

            // Apply zoom
            double zoom = (e.Delta > 0) ? ZoomStep : (1.0 / ZoomStep);
            _viewScale = Clamp(_viewScale * zoom, MinZoom, MaxZoom);

            // Update bounds with the new scale (centre unchanged for now)
            ApplyViewToWorldBounds();

            // World position under mouse AFTER zoom (same centre)
            Point worldAfter = ScreenToWorld(mouse);

            // Shift the camera so the point under the cursor stays in the same place
            _viewCenterX += (worldBefore.X - worldAfter.X);
            _viewCenterY += (worldBefore.Y - worldAfter.Y);

            ApplyViewToWorldBounds();

            // Option A: refresh points for the visible window
            ResamplePointsForCurrentView();

            e.Handled = true;
        }

        /// <summary>
        /// Start panning (left mouse drag).
        /// </summary>
        private void PlotCanvas_MouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            if (PlotCanvas == null || _points.Count == 0)
                return;

            PlotCanvas.Focus();

            _isPanning = true;
            _panStartMouse = e.GetPosition(PlotCanvas);
            _panStartCenterX = _viewCenterX;
            _panStartCenterY = _viewCenterY;

            PlotCanvas.CaptureMouse();
            PlotCanvas.Cursor = Cursors.Hand;
            e.Handled = true;
        }

        /// <summary>
        /// Stop panning.
        /// </summary>
        private void PlotCanvas_MouseLeftButtonUp(object sender, MouseButtonEventArgs e)
        {
            if (!_isPanning)
                return;

            _isPanning = false;
            PlotCanvas.ReleaseMouseCapture();
            PlotCanvas.Cursor = Cursors.Arrow;
            e.Handled = true;
        }

        /// <summary>
        /// Update camera centre while dragging to pan.
        /// We debounce the backend call, otherwise it would be too spammy during a drag.
        /// </summary>
        private void PlotCanvas_MouseMove(object sender, MouseEventArgs e)
        {
            if (!_isPanning || PlotCanvas == null)
                return;

            Point current = e.GetPosition(PlotCanvas);
            Vector deltaPx = current - _panStartMouse;

            double width = PlotCanvas.ActualWidth;
            double height = PlotCanvas.ActualHeight;

            double usableWidth = width - PlotPaddingLeft - PlotPaddingRight;
            double usableHeight = height - PlotPaddingTop - PlotPaddingBottom;

            double rangeX = _worldMaxX - _worldMinX;
            double rangeY = _worldMaxY - _worldMinY;

            if (usableWidth <= 0 || usableHeight <= 0 || rangeX <= 0 || rangeY <= 0)
                return;

            // Convert pixel delta to world delta using the current view.
            double dxWorld = deltaPx.X * (rangeX / usableWidth);
            double dyWorld = deltaPx.Y * (rangeY / usableHeight);

            // "Grab and drag": move the camera opposite to the mouse drag.
            _viewCenterX = _panStartCenterX - dxWorld;
            _viewCenterY = _panStartCenterY + dyWorld; // screen Y increases downward

            ApplyViewToWorldBounds();

            // Debounce the resample - we only refresh once the user pauses.
            _resampleTimer.Stop();
            _resampleTimer.Start();

            DrawPlot();
            e.Handled = true;
        }

        /// <summary>
        /// Right-click resets the view to the fitted data window.
        /// </summary>
        private void PlotCanvas_MouseRightButtonDown(object sender, MouseButtonEventArgs e)
        {
            if (_points.Count == 0)
                return;

            ResetViewToFit();
            DrawPlot();
            e.Handled = true;
        }

        // ===================== Plot Loading (Fit to New Data) =====================

        /// <summary>
        /// Replaces the current plot data with a new set of points,
        /// then fits the view and redraws.
        /// </summary>
        public void PlotFromCoordinates(IEnumerable<Point> points)
        {
            _points.Clear();

            if (points != null)
            {
                _points.AddRange(points);
            }

            FitWorldToPoints();
            DrawPlot();

            // One-shot resample so the initial plot uses the same dx logic as pan/zoom.
            // Dispatcher makes sure PlotCanvas.ActualWidth/ActualHeight are valid.
            Dispatcher.BeginInvoke(new Action(() =>
            {
                ResamplePointsForCurrentView();
            }), System.Windows.Threading.DispatcherPriority.Background);
        }


        /// <summary>
        /// Computes the fitted bounds so all points are visible with a small margin.
        /// Then resets the camera to the fitted view.
        /// </summary>
        private void FitWorldToPoints()
        {
            if (_points.Count == 0)
            {
                _fitMinX = -10.0;
                _fitMaxX = 10.0;
                _fitMinY = -10.0;
                _fitMaxY = 10.0;

                ResetViewToFit();
                return;
            }

            double minX = _points.Min(p => p.X);
            double maxX = _points.Max(p => p.X);
            double minY = _points.Min(p => p.Y);
            double maxY = _points.Max(p => p.Y);

            // Handle nearly-flat functions so they are still visible.
            if (Math.Abs(maxX - minX) < 1e-9)
            {
                minX -= 1.0;
                maxX += 1.0;
            }

            if (Math.Abs(maxY - minY) < 1e-9)
            {
                minY -= 1.0;
                maxY += 1.0;
            }

            // Add a margin so the curve is not tight against the edges.
            double padX = (maxX - minX) * 0.10;
            double padY = (maxY - minY) * 0.10;

            _fitMinX = minX - padX;
            _fitMaxX = maxX + padX;
            _fitMinY = minY - padY;
            _fitMaxY = maxY + padY;

            ResetViewToFit();
        }

        // ===================== Drawing Helpers: Ticks, Grid, Axes =====================

        private void DrawXTicks(
            double width,
            double height,
            Func<double, double> toCanvasX,
            Func<double, double> toCanvasY)
        {
            const int tickCount = 10;
            double step = (_worldMaxX - _worldMinX) / tickCount;

            bool xAxisVisible = (_worldMinY <= 0 && 0 <= _worldMaxY);
            double yAxisLine = xAxisVisible ? toCanvasY(0) : toCanvasY(_worldMinY);

            for (int i = 0; i <= tickCount; i++)
            {
                double xValue = _worldMinX + i * step;
                double x = toCanvasX(xValue);

                PlotCanvas.Children.Add(new Line
                {
                    X1 = x,
                    Y1 = yAxisLine - 5,
                    X2 = x,
                    Y2 = yAxisLine + 5,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                });

                var label = new TextBlock
                {
                    Text = xValue.ToString("0.##"),
                    FontSize = 12
                };

                double labelWidth = 30;
                double labelX = x - (labelWidth / 2);
                double labelY = yAxisLine + 8;

                labelX = Clamp(labelX, 0, width - labelWidth);
                labelY = Clamp(labelY, 0, height - 20);

                Canvas.SetLeft(label, labelX);
                Canvas.SetTop(label, labelY);
                PlotCanvas.Children.Add(label);
            }
        }

        private void DrawYTicks(
            double width,
            double height,
            Func<double, double> toCanvasX,
            Func<double, double> toCanvasY)
        {
            const int tickCount = 10;
            double step = (_worldMaxY - _worldMinY) / tickCount;

            bool yAxisVisible = (_worldMinX <= 0 && 0 <= _worldMaxX);
            double xAxisLine = yAxisVisible ? toCanvasX(0) : toCanvasX(_worldMinX);

            for (int i = 0; i <= tickCount; i++)
            {
                double yValue = _worldMinY + i * step;
                double y = toCanvasY(yValue);

                PlotCanvas.Children.Add(new Line
                {
                    X1 = xAxisLine - 5,
                    Y1 = y,
                    X2 = xAxisLine + 5,
                    Y2 = y,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                });

                var label = new TextBlock
                {
                    Text = yValue.ToString("0.##"),
                    FontSize = 12
                };

                double labelWidth = 40;
                double labelX = xAxisLine - (labelWidth + 8);
                double labelY = y - 8;

                if (labelX < 0)
                {
                    labelX = xAxisLine + 8;
                }

                labelX = Clamp(labelX, 0, width - labelWidth);
                labelY = Clamp(labelY, 0, height - 20);

                Canvas.SetLeft(label, labelX);
                Canvas.SetTop(label, labelY);
                PlotCanvas.Children.Add(label);
            }
        }

        private void DrawVerticalGridLines(
            double width,
            double height,
            Func<double, double> toCanvasX,
            Func<double, double> toCanvasY)
        {
            const int gridCount = 10;
            double step = (_worldMaxX - _worldMinX) / gridCount;

            for (int i = 0; i <= gridCount; i++)
            {
                double xValue = _worldMinX + i * step;
                double x = toCanvasX(xValue);

                PlotCanvas.Children.Add(new Line
                {
                    X1 = x,
                    Y1 = 0,
                    X2 = x,
                    Y2 = height,
                    Stroke = Brushes.LightGray,
                    StrokeThickness = 1,
                    StrokeDashArray = new DoubleCollection { 2, 2 }
                });
            }
        }

        private void DrawHorizontalGridLines(
            double width,
            double height,
            Func<double, double> toCanvasX,
            Func<double, double> toCanvasY)
        {
            const int gridCount = 10;
            double step = (_worldMaxY - _worldMinY) / gridCount;

            for (int i = 0; i <= gridCount; i++)
            {
                double yValue = _worldMinY + i * step;
                double y = toCanvasY(yValue);

                PlotCanvas.Children.Add(new Line
                {
                    X1 = 0,
                    Y1 = y,
                    X2 = width,
                    Y2 = y,
                    Stroke = Brushes.LightGray,
                    StrokeThickness = 1,
                    StrokeDashArray = new DoubleCollection { 2, 2 }
                });
            }
        }

        // ===================== Main Drawing Routine =====================

        private void DrawPlot()
        {
            if (PlotCanvas == null)
                return;

            double width = PlotCanvas.ActualWidth;
            double height = PlotCanvas.ActualHeight;

            PlotCanvas.Children.Clear();

            if (_points.Count == 0 || width <= 0 || height <= 0)
                return;

            double rangeX = _worldMaxX - _worldMinX;
            double rangeY = _worldMaxY - _worldMinY;

            if (rangeX <= 0 || rangeY <= 0)
                return;

            double usableWidth = width - PlotPaddingLeft - PlotPaddingRight;
            double usableHeight = height - PlotPaddingTop - PlotPaddingBottom;

            double scaleX = usableWidth / rangeX;
            double scaleY = usableHeight / rangeY;

            double ToCanvasX(double x) =>
                PlotPaddingLeft + (x - _worldMinX) * scaleX;

            double ToCanvasY(double y) =>
                PlotPaddingTop + usableHeight - (y - _worldMinY) * scaleY;

            DrawVerticalGridLines(width, height, ToCanvasX, ToCanvasY);
            DrawHorizontalGridLines(width, height, ToCanvasX, ToCanvasY);

            // Axes (only draw if 0 is visible).
            if (_worldMinX <= 0 && 0 <= _worldMaxX)
            {
                double x0 = ToCanvasX(0);

                PlotCanvas.Children.Add(new Line
                {
                    X1 = x0,
                    Y1 = 0,
                    X2 = x0,
                    Y2 = height,
                    Stroke = Brushes.Gray,
                    StrokeThickness = 1
                });
            }

            if (_worldMinY <= 0 && 0 <= _worldMaxY)
            {
                double y0 = ToCanvasY(0);

                PlotCanvas.Children.Add(new Line
                {
                    X1 = 0,
                    Y1 = y0,
                    X2 = width,
                    Y2 = y0,
                    Stroke = Brushes.Gray,
                    StrokeThickness = 1
                });
            }

            DrawXTicks(width, height, ToCanvasX, ToCanvasY);
            DrawYTicks(width, height, ToCanvasX, ToCanvasY);

            // Convert the world points into canvas points (and filter out invalid values).
            var xs = new List<double>();
            var ys = new List<double>();

            foreach (var p in _points)
            {
                double cx = ToCanvasX(p.X);
                double cy = ToCanvasY(p.Y);

                if (double.IsNaN(cx) || double.IsNaN(cy) ||
                    double.IsInfinity(cx) || double.IsInfinity(cy))
                    continue;

                xs.Add(cx);
                ys.Add(cy);
            }

            // Not enough points for spline interpolation, so just draw a basic polyline.
            if (xs.Count < 3)
            {
                var fallback = new Polyline
                {
                    Stroke = Brushes.Blue,
                    StrokeThickness = 2,
                    Clip = new RectangleGeometry(new Rect(0, 0, width, height))
                };

                for (int i = 0; i < xs.Count; i++)
                    fallback.Points.Add(new Point(xs[i], ys[i]));

                PlotCanvas.Children.Add(fallback);
                return;
            }

            int interpCount = (int)Math.Max(200, Math.Min(5000, width));

            (double[] xsOut, double[] ysOut) = Cubic.InterpolateXY(xs.ToArray(), ys.ToArray(), interpCount);

            var smooth = new Polyline
            {
                Stroke = Brushes.Blue,
                StrokeThickness = 2,
                Clip = new RectangleGeometry(new Rect(0, 0, width, height))
            };

            for (int i = 0; i < xsOut.Length; i++)
            {
                smooth.Points.Add(new Point(xsOut[i], ysOut[i]));
            }

            PlotCanvas.Children.Add(smooth);
        }
    }

    // =====================================================================
    // Cubic spline interpolation (Scott W Harden approach)
    // Source reference: https://swharden.com/blog/2022-01-22-spline-interpolation/
    // This is an adaptation of original work by Ryan Seghers (links in the blog).
    // =====================================================================

    /// <summary>
    /// Cubic provides spline interpolation for smoothing the curve on-screen.
    /// The input points are spaced unevenly, then resampled to a smooth set of output points.
    /// </summary>
    public static class Cubic
    {
        public static (double[] xs, double[] ys) InterpolateXY(double[] xs, double[] ys, int count)
        {
            if (xs is null || ys is null || xs.Length != ys.Length)
                throw new ArgumentException($"{nameof(xs)} and {nameof(ys)} must have same length");

            int inputPointCount = xs.Length;

            double[] inputDistances = new double[inputPointCount];
            for (int i = 1; i < inputPointCount; i++)
            {
                double dx = xs[i] - xs[i - 1];
                double dy = ys[i] - ys[i - 1];
                double distance = Math.Sqrt(dx * dx + dy * dy);
                inputDistances[i] = inputDistances[i - 1] + distance;
            }

            double meanDistance = inputDistances.Last() / (count - 1);
            double[] evenDistances = Enumerable.Range(0, count).Select(x => x * meanDistance).ToArray();

            double[] xsOut = Interpolate(inputDistances, xs, evenDistances);
            double[] ysOut = Interpolate(inputDistances, ys, evenDistances);

            return (xsOut, ysOut);
        }

        private static double[] Interpolate(double[] xOrig, double[] yOrig, double[] xInterp)
        {
            (double[] a, double[] b) = FitMatrix(xOrig, yOrig);

            double[] yInterp = new double[xInterp.Length];
            for (int i = 0; i < yInterp.Length; i++)
            {
                int j;
                for (j = 0; j < xOrig.Length - 2; j++)
                    if (xInterp[i] <= xOrig[j + 1])
                        break;

                double dx = xOrig[j + 1] - xOrig[j];
                double t = (xInterp[i] - xOrig[j]) / dx;

                double y = (1 - t) * yOrig[j] + t * yOrig[j + 1] +
                    t * (1 - t) * (a[j] * (1 - t) + b[j] * t);

                yInterp[i] = y;
            }

            return yInterp;
        }

        private static (double[] a, double[] b) FitMatrix(double[] x, double[] y)
        {
            int n = x.Length;
            double[] a = new double[n - 1];
            double[] b = new double[n - 1];
            double[] r = new double[n];
            double[] A = new double[n];
            double[] B = new double[n];
            double[] C = new double[n];

            double dx1, dx2, dy1, dy2;

            dx1 = x[1] - x[0];
            C[0] = 1.0f / dx1;
            B[0] = 2.0f * C[0];
            r[0] = 3 * (y[1] - y[0]) / (dx1 * dx1);

            for (int i = 1; i < n - 1; i++)
            {
                dx1 = x[i] - x[i - 1];
                dx2 = x[i + 1] - x[i];
                A[i] = 1.0f / dx1;
                C[i] = 1.0f / dx2;
                B[i] = 2.0f * (A[i] + C[i]);
                dy1 = y[i] - y[i - 1];
                dy2 = y[i + 1] - y[i];
                r[i] = 3 * (dy1 / (dx1 * dx1) + dy2 / (dx2 * dx2));
            }

            dx1 = x[n - 1] - x[n - 2];
            dy1 = y[n - 1] - y[n - 2];
            A[n - 1] = 1.0f / dx1;
            B[n - 1] = 2.0f * A[n - 1];
            r[n - 1] = 3 * (dy1 / (dx1 * dx1));

            double[] cPrime = new double[n];
            cPrime[0] = C[0] / B[0];
            for (int i = 1; i < n; i++)
                cPrime[i] = C[i] / (B[i] - cPrime[i - 1] * A[i]);

            double[] dPrime = new double[n];
            dPrime[0] = r[0] / B[0];
            for (int i = 1; i < n; i++)
                dPrime[i] = (r[i] - dPrime[i - 1] * A[i]) / (B[i] - cPrime[i - 1] * A[i]);

            double[] k = new double[n];
            k[n - 1] = dPrime[n - 1];
            for (int i = n - 2; i >= 0; i--)
                k[i] = dPrime[i] - cPrime[i] * k[i + 1];

            for (int i = 1; i < n; i++)
            {
                dx1 = x[i] - x[i - 1];
                dy1 = y[i] - y[i - 1];
                a[i - 1] = k[i - 1] * dx1 - dy1;
                b[i - 1] = -k[i] * dx1 + dy1;
            }

            return (a, b);
        }
    }
}
