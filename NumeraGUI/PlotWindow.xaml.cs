/*
 * PlotWindow.xaml.cs
 * ------------------
 * Plotting window for the Numera GUI.
 *
 * This window:
 *  - Receives graph commands from the user (e.g. "graph x = x^2, (-5, 5);")
 *  - Requests evaluated (x, y) points from the F# interpreter
 *  - Automatically scales and draws the plot
 *  - Displays current variables and errors in-panel (no popups)
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

        // Stores the currently plotted points
        private readonly List<Point> _points = new List<Point>();

        // World-coordinate bounds
        private double _worldMinX = -10.0;
        private double _worldMaxX = 10.0;
        private double _worldMinY = -10.0;
        private double _worldMaxY = 10.0;

        /*
         * Delegate supplied by MainWindow.
         * Given a graph expression string, returns a list of points.
         */
        public Func<string, IEnumerable<Point>> GetPointsForExpression { get; set; }

        public PlotWindow()
        {
            InitializeComponent();

            // Redraw plot when canvas resizes
            if (PlotCanvas != null)
            {
                PlotCanvas.SizeChanged += (s, e) => DrawPlot();
            }
        }

        /* ===================== PUBLIC API ===================== */

        /// <summary>
        /// Allows MainWindow to update the Current Variables box.
        /// Call this whenever interpreter variables change.
        /// </summary>
        public void SetVariablesText(string variablesText)
        {
            if (VariablesBox != null)
            {
                VariablesBox.Text = variablesText ?? string.Empty;
            }
        }

        /* ===================== ERROR HANDLING ===================== */

        private void ShowError(string message)
        {
            if (ErrorBox != null)
            {
                ErrorBox.Text = message ?? string.Empty;
            }
        }

        private void ClearError()
        {
            ShowError(string.Empty);
        }

        /* ===================== WINDOW EVENTS ===================== */

        private void PlotWindow_Loaded(object sender, RoutedEventArgs e)
        {
            DrawPlot();
        }

        private void HelpButton_Click(object sender, RoutedEventArgs e)
        {
            var helpWindow = new PlotHelpWindow
            {
                Owner = this
            };

            helpWindow.ShowDialog();
        }

        /* ===================== BUTTON HANDLERS ===================== */

        private void PlotButton_Click(object sender, RoutedEventArgs e)
        {
            ClearError();

            if (GetPointsForExpression == null)
            {
                ShowError("Error: Plotting is not connected to the interpreter.");
                return;
            }

            string expr = FunctionTextBox.Text;

            if (string.IsNullOrWhiteSpace(expr))
            {
                ShowError("Error: Please enter a graph expression.");
                return;
            }

            try
            {
                var points = GetPointsForExpression(expr);

                if (points == null)
                {
                    ShowError("Error: No points returned for this expression.");
                    return;
                }

                PlotFromCoordinates(points);
            }
            catch (Exception ex)
            {
                ShowError("Error: " + ex.Message);
            }
        }

        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            ClearError();

            _points.Clear();
            PlotCanvas.Children.Clear();
            FunctionTextBox.Clear();
        }

        /* ===================== PLOTTING CORE ===================== */

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

        private void FitWorldToPoints()
        {
            if (_points.Count == 0)
            {
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

            if (Math.Abs(maxX - minX) < 1e-9)
            {
                minX -= 1;
                maxX += 1;
            }

            if (Math.Abs(maxY - minY) < 1e-9)
            {
                minY -= 1;
                maxY += 1;
            }

            double padX = (maxX - minX) * 0.1;
            double padY = (maxY - minY) * 0.1;

            _worldMinX = minX - padX;
            _worldMaxX = maxX + padX;
            _worldMinY = minY - padY;
            _worldMaxY = maxY + padY;
        }

        /* ===================== DRAWING ===================== */

        private static double Clamp(double value, double min, double max)
        {
            if (value < min) return min;
            if (value > max) return max;
            return value;
        }

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

            DrawGridLines(width, height, ToCanvasX, ToCanvasY);
            DrawAxes(width, height, ToCanvasX, ToCanvasY);
            DrawTicks(width, height, ToCanvasX, ToCanvasY);
            DrawCurve(ToCanvasX, ToCanvasY);
        }

        private void DrawGridLines(
            double width,
            double height,
            Func<double, double> toCanvasX,
            Func<double, double> toCanvasY)
        {
            const int gridCount = 10;

            for (int i = 0; i <= gridCount; i++)
            {
                double t = (double)i / gridCount;

                double x = toCanvasX(_worldMinX + t * (_worldMaxX - _worldMinX));
                double y = toCanvasY(_worldMinY + t * (_worldMaxY - _worldMinY));

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

        private void DrawAxes(
            double width,
            double height,
            Func<double, double> toCanvasX,
            Func<double, double> toCanvasY)
        {
            if (_worldMinX <= 0 && 0 <= _worldMaxX)
            {
                double x0 = toCanvasX(0);
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
                double y0 = toCanvasY(0);
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
        }

        private void DrawTicks(
            double width,
            double height,
            Func<double, double> toCanvasX,
            Func<double, double> toCanvasY)
        {
            const int tickCount = 10;

            for (int i = 0; i <= tickCount; i++)
            {
                double t = (double)i / tickCount;

                double xVal = _worldMinX + t * (_worldMaxX - _worldMinX);
                double yVal = _worldMinY + t * (_worldMaxY - _worldMinY);

                double x = toCanvasX(xVal);
                double y = toCanvasY(yVal);

                PlotCanvas.Children.Add(new Line
                {
                    X1 = x,
                    Y1 = toCanvasY(0) - 5,
                    X2 = x,
                    Y2 = toCanvasY(0) + 5,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                });

                PlotCanvas.Children.Add(new Line
                {
                    X1 = toCanvasX(0) - 5,
                    Y1 = y,
                    X2 = toCanvasX(0) + 5,
                    Y2 = y,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                });

                AddLabel(xVal.ToString("0.##"), x - 15, toCanvasY(0) + 8);
                AddLabel(yVal.ToString("0.##"), toCanvasX(0) - 45, y - 8);
            }
        }

        private void AddLabel(string text, double x, double y)
        {
            var label = new TextBlock
            {
                Text = text,
                FontSize = 12
            };

            Canvas.SetLeft(label, x);
            Canvas.SetTop(label, y);
            PlotCanvas.Children.Add(label);
        }

        private void DrawCurve(Func<double, double> toCanvasX,
                               Func<double, double> toCanvasY)
        {
            var polyline = new Polyline
            {
                Stroke = Brushes.Blue,
                StrokeThickness = 2
            };

            foreach (var p in _points)
            {
                polyline.Points.Add(new Point(
                    toCanvasX(p.X),
                    toCanvasY(p.Y)));
            }

            PlotCanvas.Children.Add(polyline);
        }
    }
}
