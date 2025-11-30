using System;
using System.Windows;

namespace NumeraGUI
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            RefreshVariables();
        }

        private void RunButton_Click(object sender, RoutedEventArgs e)
        {
            var input = InputBox.Text;

            try
            {
                // Evaluate using F# interpreter
                var result = FSharpLib.Interpreter.Evaluate(input);

                if (!string.IsNullOrEmpty(result) &&
                    result.StartsWith("Error:", StringComparison.OrdinalIgnoreCase))
                {
                    // Show errors only in ErrorBox
                    ErrorBox.Text = result;
                    OutputBox.Text = string.Empty;
                }
                else
                {
                    // Show successful result only in OutputBox
                    ErrorBox.Text = string.Empty;
                    OutputBox.Text = result;
                }

                // Update variables panel
                RefreshVariables();
            }
            catch (Exception ex)
            {
                // Hard errors go to ErrorBox
                ErrorBox.Text = "Error: " + ex.Message;
            }
        }

        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            InputBox.Clear();
            OutputBox.Clear();
            ErrorBox.Clear();
        }

        private void HelpButton_Click(object sender, RoutedEventArgs e)
        {
            var helpWindow = new HelpWindow
            {
                Owner = this
            };
            helpWindow.ShowDialog();
        }

        // This just opens the plotting window; no plotting logic
        private void PlottingButton_Click(object sender, RoutedEventArgs e)
        {
            var plotWindow = new PlotWindow
            {
                Owner = this
            };
            plotWindow.Show();
        }

        private void RefreshVariables()
        {
            try
            {
                var variablesText = FSharpLib.Interpreter.GetVariables();
                VariablesBox.Text = variablesText;
            }
            catch
            {
                // Ignore variable refresh issues so the app doesn't crash
            }
        }
    }
}
