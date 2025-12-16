/*
* HelpWindow.xaml.cs
* ------------------
* This file controls the Help window for the Numera application.
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
    public partial class HelpWindow : Window
    {
        public HelpWindow()
        {
            // Initialise the Help window UI defined in HelpWindow.xaml
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
