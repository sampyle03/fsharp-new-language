/*
* PlotHelpWindow.xaml.cs
* ------------------
* This file controls the Help window for plotting.
*
* The Help window is a simple, modal dialog that displays
* instructions and usage information for the user.
*
* It does not interact with the interpreter or application state;
* it purely provides guidance and can be closed by the user.
*/

using System.Windows;

namespace NumeraGUI
{
    /// <summary>
    /// Interaction logic for PlotHelpWindow.xaml
    /// </summary>
    public partial class PlotHelpWindow : Window
    {
        public PlotHelpWindow()
        {
            InitializeComponent();
        }

        /// <summary>
        /// Handles the Close button click.
        /// Simply closes the help window and returns control
        /// to the main application window.
        /// </summary>
        private void CloseButton_Click(object sender, RoutedEventArgs e)
        {
            this.Close();
        }
    }
}
