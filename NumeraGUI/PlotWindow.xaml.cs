using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Shapes;

namespace NumeraGUI
{
    public partial class PlotWindow : Window
    {
        private const double PlotMargin = 30.0;

        public PlotWindow()
        {
            InitializeComponent();
            Loaded += PlotWindow_Loaded;
        }

        private void PlotWindow_Loaded(object sender, RoutedEventArgs e)
        {
            DrawAxes();
        }

        private void HelpButton_Click(object sender, RoutedEventArgs e)
        {
            MessageBox.Show(
                "Use this window to plot functions of x.\n\n" +
                "Enter f(x), choose an x-range, then press Plot.\n" +
                "Later this will use the Numera interpreter to evaluate the function.",
                "Plotting Help",
                MessageBoxButton.OK,
                MessageBoxImage.Information);
        }

        private void ClearPlot()
        {
            PlotCanvas.Children.Clear();
        }

        private void DrawAxes()
        {
            double width = PlotCanvas.ActualWidth;
            double height = PlotCanvas.ActualHeight;

            if (width <= 0) width = 400;
            if (height <= 0) height = 300;

            var xAxis = new Line
            {
                X1 = PlotMargin,
                Y1 = height - PlotMargin,
                X2 = width - PlotMargin,
                Y2 = height - PlotMargin,
                Stroke = Brushes.Black,
                StrokeThickness = 1
            };

            var yAxis = new Line
            {
                X1 = PlotMargin,
                Y1 = PlotMargin,
                X2 = PlotMargin,
                Y2 = height - PlotMargin,
                Stroke = Brushes.Black,
                StrokeThickness = 1
            };

            PlotCanvas.Children.Add(xAxis);
            PlotCanvas.Children.Add(yAxis);
        }

        public void PlotCurve(IEnumerable<Point> points)
        {
            var pts = points.ToList();
            if (!pts.Any())
                return;

            ClearPlot();

            double width = PlotCanvas.ActualWidth;
            double height = PlotCanvas.ActualHeight;

            if (width <= 0) width = 400;
            if (height <= 0) height = 300;

            double minX = pts.Min(p => p.X);
            double maxX = pts.Max(p => p.X);
            double minY = pts.Min(p => p.Y);
            double maxY = pts.Max(p => p.Y);

            if (Math.Abs(maxX - minX) < 1e-9) maxX = minX + 1;
            if (Math.Abs(maxY - minY) < 1e-9) maxY = minY + 1;

            double xScale = (width - 2 * PlotMargin) / (maxX - minX);
            double yScale = (height - 2 * PlotMargin) / (maxY - minY);

            var polyline = new Polyline
            {
                Stroke = Brushes.Blue,
                StrokeThickness = 2
            };

            foreach (var p in pts)
            {
                double sx = PlotMargin + (p.X - minX) * xScale;
                double sy = height - (PlotMargin + (p.Y - minY) * yScale);
                polyline.Points.Add(new System.Windows.Point(sx, sy));
            }

            DrawAxes();
            PlotCanvas.Children.Add(polyline);
        }

        public void PlotLine(double a, double b, double xStart, double xEnd, int samples = 200)
        {
            var pts = new List<Point>();
            double step = (xEnd - xStart) / (samples - 1);

            for (int i = 0; i < samples; i++)
            {
                double x = xStart + i * step;
                double y = a * x + b;
                pts.Add(new Point(x, y));
            }

            PlotCurve(pts);
        }

        private void ClearPlotButton_Click(object sender, RoutedEventArgs e)
        {
            ClearPlot();
            DrawAxes();
        }

        private void PlotButton_Click(object sender, RoutedEventArgs e)
        {
            // For now we only use the range and samples.
            // The FunctionTextBox content will be used once F# integration is added.

            if (!double.TryParse(XFromTextBox.Text, out double xMin))
            {
                MessageBox.Show("Invalid value for 'x from'.", "Input error");
                return;
            }

            if (!double.TryParse(XToTextBox.Text, out double xMax))
            {
                MessageBox.Show("Invalid value for 'x to'.", "Input error");
                return;
            }

            if (xMax <= xMin)
            {
                MessageBox.Show("'x to' must be greater than 'x from'.", "Input error");
                return;
            }

            // Placeholder: plot a simple line using the chosen range.
            // Later: call into F# using FunctionTextBox.Text to evaluate the function.
            PlotLine(1.0, 0.0, xMin, xMax);
        }
    }
}
