/*
* PlotWindow.xaml.cs
* ------------------
* This file is responsible for drawing function plots in the Numera GUI.
*
* The window receives a list of (x, y) points from the F# interpreter
* (generated from a command such as: graph x = x^2, (-5, 5);),
* and then:
*  - automatically scales the axes to fit the data,
*  - draws the x- and y-axes,
*  - adds tick marks and numeric labels,
*  - and renders the function curve on a Canvas.
*
* Smooth curve fitting (spline interpolation):
* The curve is rendered using cubic spline interpolation based on the approach
* by Scott W. Harden (Jan 22, 2022), which adapts original work by
* Ryan Seghers. See:
* https://swharden.com/blog/2022-01-22-spline-interpolation/
*/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;

namespace NumeraGUI
{
    public partial class PlotWindow : Window
    {
        // Padding around the plot area so axis labels do not overlap the border
        private const double PlotPaddingLeft = 50;
        private const double PlotPaddingRight = 20;
        private const double PlotPaddingTop = 20;
        private const double PlotPaddingBottom = 40;

        // Stores the current set of points being plotted (world coordinates)
        private readonly List<Point> _points = new List<Point>();

        // World-coordinate bounds used to scale the plot.
        // These are automatically updated to fit the current data.
        private double _worldMinX = -10.0;
        private double _worldMaxX = 10.0;
        private double _worldMinY = -10.0;
        private double _worldMaxY = 10.0;

        // Delegate that is set by MainWindow.
        // Given a graph command string, this returns a list of points from F#.
        public Func<string, IEnumerable<Point>> GetPointsForExpression { get; set; }

        public PlotWindow()
        {
            InitializeComponent();

            // Redraw the plot whenever the window is resized
            if (PlotCanvas != null)
            {
                PlotCanvas.SizeChanged += (s, e) => DrawPlot();
            }
        }

        /// <summary>
        /// Utility function to clamp a value between a minimum and maximum.
        /// This is mainly used to keep labels inside the canvas bounds.
        /// </summary>
        private static double Clamp(double v, double min, double max)
        {
            if (v < min) return min;
            if (v > max) return max;
            return v;
        }

        /// <summary>
        /// Called when the window finishes loading.
        /// If any points already exist, this ensures they are drawn.
        /// </summary>
        private void PlotWindow_Loaded(object sender, RoutedEventArgs e)
        {
            DrawPlot();
        }

        /// <summary>
        /// Handles the Plot button.
        /// Reads the expression from the text box, asks F# to generate points,
        /// and then updates the plot.
        /// </summary>
        private void PlotButton_Click(object sender, RoutedEventArgs e)
        {
            // If the F# delegate has not been wired up, do nothing
            if (GetPointsForExpression == null)
                return;

            string expr = FunctionTextBox.Text;

            if (string.IsNullOrWhiteSpace(expr))
                return;

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
            // Clear plotted data
            _points.Clear();

            // Clear anything drawn on the canvas
            PlotCanvas.Children.Clear();

            // Clear the input expression box
            FunctionTextBox.Clear();
        }

        /// <summary>
        /// Replaces the current plot data with a new set of points,
        /// then rescales and redraws the plot.
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
        }

        /// <summary>
        /// Automatically adjusts the world-coordinate bounds
        /// so that all points are visible with a small margin.
        /// </summary>
        private void FitWorldToPoints()
        {
            if (_points.Count == 0)
            {
                // Default bounds when nothing is plotted
                _worldMinX = -10.0;
                _worldMaxX = 10.0;
                _worldMinY = -10.0;
                _worldMaxY = 10.0;
                return;
            }

            double minX = _points.Min(p => p.X);
            double maxX = _points.Max(p => p.X);
            double minY = _points.Min(p => p.Y);
            double maxY = _points.Max(p => p.Y);

            // Handle nearly-flat functions so they are still visible
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

            // Add a small margin so the curve is not tight against the edges
            double padX = (maxX - minX) * 0.10;
            double padY = (maxY - minY) * 0.10;

            _worldMinX = minX - padX;
            _worldMaxX = maxX + padX;
            _worldMinY = minY - padY;
            _worldMaxY = maxY + padY;
        }

        /// <summary>
        /// Draws tick marks and numeric labels along the x-axis.
        /// Labels are positioned near the x-axis line where possible.
        /// </summary>
        private void DrawXTicks(
            double width,
            double height,
            Func<double, double> toCanvasX,
            Func<double, double> toCanvasY)
        {
            const int tickCount = 10;
            double step = (_worldMaxX - _worldMinX) / tickCount;

            // Determine where the x-axis is drawn on the canvas
            bool xAxisVisible = (_worldMinY <= 0 && 0 <= _worldMaxY);
            double yAxisLine = xAxisVisible ? toCanvasY(0) : toCanvasY(_worldMinY);

            for (int i = 0; i <= tickCount; i++)
            {
                double xValue = _worldMinX + i * step;
                double x = toCanvasX(xValue);

                // Tick mark
                PlotCanvas.Children.Add(new Line
                {
                    X1 = x,
                    Y1 = yAxisLine - 5,
                    X2 = x,
                    Y2 = yAxisLine + 5,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                });

                // Numeric label
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

        /// <summary>
        /// Draws tick marks and numeric labels along the y-axis.
        /// Labels are placed next to the y-axis line where possible.
        /// </summary>
        private void DrawYTicks(
            double width,
            double height,
            Func<double, double> toCanvasX,
            Func<double, double> toCanvasY)
        {
            const int tickCount = 10;
            double step = (_worldMaxY - _worldMinY) / tickCount;

            // Determine where the y-axis is drawn on the canvas
            bool yAxisVisible = (_worldMinX <= 0 && 0 <= _worldMaxX);
            double xAxisLine = yAxisVisible ? toCanvasX(0) : toCanvasX(_worldMinX);

            for (int i = 0; i <= tickCount; i++)
            {
                double yValue = _worldMinY + i * step;
                double y = toCanvasY(yValue);

                // Tick mark
                PlotCanvas.Children.Add(new Line
                {
                    X1 = xAxisLine - 5,
                    Y1 = y,
                    X2 = xAxisLine + 5,
                    Y2 = y,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                });

                // Numeric label
                var label = new TextBlock
                {
                    Text = yValue.ToString("0.##"),
                    FontSize = 12
                };

                double labelWidth = 40;
                double labelX = xAxisLine - (labelWidth + 8);
                double labelY = y - 8;

                // If the axis is too close to the left edge, move labels to the right
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

        /// <summary>
        /// Draws vertical grid lines aligned with the x-axis tick marks.
        /// These lines run the full height of the usable plot area.
        /// </summary>
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

        /// <summary>
        /// Draws horizontal grid lines aligned with the y-axis tick marks.
        /// These lines run the full width of the usable plot area.
        /// </summary>
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

        /// <summary>
        /// Main drawing routine.
        /// Clears the canvas, draws axes, ticks, and then renders the function curve.
        /// </summary>
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

            // Work out how much space we actually have for the plot
            double usableWidth = width - PlotPaddingLeft - PlotPaddingRight;
            double usableHeight = height - PlotPaddingTop - PlotPaddingBottom;

            double scaleX = usableWidth / rangeX;
            double scaleY = usableHeight / rangeY;

            // Convert world coordinates into canvas coordinates
            double ToCanvasX(double x) =>
                PlotPaddingLeft + (x - _worldMinX) * scaleX;

            double ToCanvasY(double y) =>
                PlotPaddingTop + usableHeight - (y - _worldMinY) * scaleY;

            // Draw grid lines first so everything else appears on top
            DrawVerticalGridLines(width, height, ToCanvasX, ToCanvasY);
            DrawHorizontalGridLines(width, height, ToCanvasX, ToCanvasY);

            // Draw y-axis (x = 0) if visible
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

            // Draw x-axis (y = 0) if visible
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

            // Draw tick marks and numeric labels
            DrawXTicks(width, height, ToCanvasX, ToCanvasY);
            DrawYTicks(width, height, ToCanvasX, ToCanvasY);

            // ===================== SMOOTH CURVE (Spline Interpolation) =====================
            // Convert points to canvas coordinates, skipping invalid points.
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

            // If too few points for interpolation, fall back to a normal polyline.
            if (xs.Count < 3)
            {
                var fallback = new Polyline { Stroke = Brushes.Blue, StrokeThickness = 2 };
                for (int i = 0; i < xs.Count; i++)
                    fallback.Points.Add(new Point(xs[i], ys[i]));
                PlotCanvas.Children.Add(fallback);
                return;
            }

            // Choose a reasonable number of interpolated points:
            // - at least 200 so it looks smooth
            // - capped to avoid UI slowdown
            int interpCount = (int)Math.Max(200, Math.Min(5000, width));

            (double[] xsOut, double[] ysOut) = Cubic.InterpolateXY(xs.ToArray(), ys.ToArray(), interpCount);

            var smooth = new Polyline
            {
                Stroke = Brushes.Blue,
                StrokeThickness = 2
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
    public static class Cubic
    {
        /// <summary>
        /// Generate a smooth (interpolated) curve that follows the path of the given X/Y points.
        /// </summary>
        public static (double[] xs, double[] ys) InterpolateXY(double[] xs, double[] ys, int count)
        {
            if (xs is null || ys is null || xs.Length != ys.Length)
                throw new ArgumentException($"{nameof(xs)} and {nameof(ys)} must have same length");

            int inputPointCount = xs.Length;

            // Distances along the curve (arc-length parameterisation)
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

            // Solve the tridiagonal system
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
