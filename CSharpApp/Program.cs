using System;
using FSharpLib;

class Program
{
       static void Main()
    {
        // Numera is the module inside the F# Program1.fs file and namespace
        //Console.Write("Enter a line of text:");
        //var line = Convert.ToInt32(Console.ReadLine());
        //Console.Write("Enter a line of text2 :");
        //var line2 = Convert.ToInt32(Console.ReadLine());

        //Console.WriteLine( "Hey there", line + line2);

        //// Tokenize the input line
        //Console.WriteLine(Numera.add(line, line2));

        String[] strings = { "Hello", "from", "C#" };

        Console.WriteLine(Numera.main(strings));
    }
}