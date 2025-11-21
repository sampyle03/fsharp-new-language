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
                var result = FSharpLib.Interpreter.Evaluate(input);
                OutputBox.Text = result;
                ErrorBox.Text = "";
                RefreshVariables();
            }
            catch (Exception ex)
            {
                ErrorBox.Text = ex.Message;
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
            var helpWindow = new HelpWindow();
            helpWindow.Show();
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
