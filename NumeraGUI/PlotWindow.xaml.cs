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
*  - and renders the function as a polyline on a Canvas.
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

        // Stores the current set of points being plotted
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
            MessageBox.Show(
                "Enter an expression in the format:\n\n" +
                "  graph x = x^2, (-5, 5);\n\n" +
                "Then click 'Plot' to see its graph.",
                "Plot Help",
                MessageBoxButton.OK,
                MessageBoxImage.Information);
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
        /// Labels are placed next to the y-axis line when possible.
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

            // Draw the function curve
            var polyline = new Polyline
            {
                Stroke = Brushes.Blue,
                StrokeThickness = 2
            };

            foreach (var p in _points)
            {
                polyline.Points.Add(new Point(ToCanvasX(p.X), ToCanvasY(p.Y)));
            }

            PlotCanvas.Children.Add(polyline);
        }
    }
}
