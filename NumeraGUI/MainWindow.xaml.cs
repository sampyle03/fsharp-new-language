/*
* MainWindow.xaml.cs
* ------------------
* This file controls the main Numera application window.
*
* It is responsible for:
*  - reading user input from the input box,
*  - sending that input to the F# interpreter for evaluation,
*  - displaying results, errors, and variables in the GUI,
*  - opening the plotting window when requested.
*/

using System;
using System.Linq;
using System.Windows;

namespace NumeraGUI
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            // Populate the variables panel when the application first loads
            RefreshVariables();
        }

        /// <summary>
        /// Handles the Run button.
        /// Sends the input text to the F# interpreter and displays either
        /// the result or any error message returned.
        /// </summary>
        private void RunButton_Click(object sender, RoutedEventArgs e)
        {
            var input = InputBox.Text;

            try
            {
                // Evaluate the input using the F# interpreter
                var result = FSharpLib.Interpreter.Evaluate(input);

                // The interpreter reports errors as strings starting with "Error:"
                if (!string.IsNullOrEmpty(result) &&
                    result.StartsWith("Error:", StringComparison.OrdinalIgnoreCase))
                {
                    // Errors are shown only in the error box
                    ErrorBox.Text = result;
                    OutputBox.Text = string.Empty;
                }
                else
                {
                    // Successful results are shown only in the output box
                    ErrorBox.Text = string.Empty;
                    OutputBox.Text = result;
                }

                // Refresh the variables panel after every evaluation
                RefreshVariables();
            }
            catch (Exception ex)
            {
                // Any unexpected exceptions are treated as hard errors
                ErrorBox.Text = "Error: " + ex.Message;
            }
        }

        /// <summary>
        /// Clears all text boxes in the main window.
        /// This does not reset the interpreter state or variables.
        /// </summary>
        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            InputBox.Clear();
            OutputBox.Clear();
            ErrorBox.Clear();
        }

        /// <summary>
        /// Opens the help window as a modal dialog.
        /// </summary>
        private void HelpButton_Click(object sender, RoutedEventArgs e)
        {
            var helpWindow = new HelpWindow
            {
                Owner = this
            };

            helpWindow.ShowDialog();
        }

        /// <summary>
        /// Opens the plotting window.
        /// This method does not perform any plotting itself; it simply
        /// wires the plotting window to the F# interpreter and shows it.
        /// </summary>
        private void PlottingButton_Click(object sender, RoutedEventArgs e)
        {
            var plotWindow = new PlotWindow
            {
                Owner = this
            };

            // Connect the plotting window to the F# plotting function.
            // The plot window passes a graph command string,
            // and receives a list of (x, y) points in return.
            plotWindow.GetPointsForExpression = expr =>
            {
                var pts = FSharpLib.Interpreter.GetPointsForExpression(expr);

                // Convert F# tuples into WPF Point objects
                return pts.Select(p => new Point(p.Item1, p.Item2));
            };

            plotWindow.Show();
        }

        /// <summary>
        /// Refreshes the variables panel by asking the F# interpreter
        /// for its current symbol table.
        /// </summary>
        private void RefreshVariables()
        {
            try
            {
                var variablesText = FSharpLib.Interpreter.GetVariables();
                VariablesBox.Text = variablesText;
            }
            catch
            {
                // Variable refresh failures are ignored so the UI remains responsive
            }
        }
    }
}
