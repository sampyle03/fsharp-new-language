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
                // Evaluate using F#
                var result = FSharpLib.Interpreter.Evaluate(input);

                if (result.StartsWith("Error:"))
                {
                    // Send error text to the error box ONLY
                    ErrorBox.Text = result;
                    OutputBox.Text = string.Empty; // clear successful output
                }
                else
                {
                    // Successful result → show in OutputBox
                    OutputBox.Text = result;
                    ErrorBox.Text = string.Empty; // clear old errors
                }
            }
            catch (Exception ex)
            {
                // Hard exceptions (rare) still go to ErrorBox
                ErrorBox.Text = "Error: " + ex.Message;
                OutputBox.Text = string.Empty;
            }

            RefreshVariables();
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

        private void RefreshVariables()
        {
            try
            {
                var variablesText = FSharpLib.Interpreter.GetVariables();
                VariablesBox.Text = variablesText;
            }
            catch
            {
                // Fail silently – no crash for variable display issues
            }
        }
    }
}
