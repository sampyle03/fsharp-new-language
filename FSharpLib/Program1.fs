// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

namespace FSharpLib

module Numera = 

    open System

    type terminal = 
        Add | Sub | Mul | Div | Rem | Cos | Sin | Tan | Lpar | Rpar | Num of int
        | Identifier of string

    let add(x:int, y:int) : int = x + y

    let str2lst s = [for c in s -> c] // Converts the input string to list of characters
    let isBlank c = System.Char.IsWhiteSpace c
    let isDigit c = System.Char.IsDigit c
    let isLetter c = System.Char.IsLetter c
    let lexError = System.Exception("Lexer error")
    let charToDigit (c:char) = (int)((int)c - (int)'0')
    let parseError = System.Exception("Parser error")

    let rec scanInt(remaining, value) = 
        // returns the integer value and the remaining characters after it
        Console.Write("{0}, {1}", remaining, value)
        match remaining with
        c :: tail when isDigit c -> scanInt(tail, 10*value+(charToDigit c))
        | _ -> (remaining, value)

    let rec scanIdentifier(remaining, identStr) =
        // returns the identifier string and the remaining characters after it
        match remaining with
        c :: tail when isLetter c -> scanIdentifier(tail, identStr + string c)
        | _ -> (remaining, identStr)

    // Lexer function
    let lexer input = 
        // Recursive scan function
        let rec scan input =
            match input with
            | [] -> []
            | '+'::tail -> Add :: scan tail
            | '-'::tail -> Sub :: scan tail
            | '*'::tail -> Mul :: scan tail
            | '/'::tail -> Div :: scan tail
            | '%'::tail -> Rem :: scan tail
            | '('::tail -> Lpar:: scan tail
            | ')'::tail -> Rpar:: scan tail
            | c :: tail when isBlank c -> scan tail
            | c :: tail when isDigit c -> let (remaining, value) = scanInt(tail, charToDigit c) 
                                          Num value :: scan remaining
            | c :: tail when isLetter c -> let (remaining, word) = scanIdentifier(tail, string c)
                                           match word.ToLower() with
                                           | "sin" -> Identifier "sin" :: scan remaining
                                           | "cos" -> Identifier "cos" :: scan remaining
                                           | "tan" -> Identifier "tan" :: scan remaining
                                           | other -> Identifier other :: scan remaining

            | _ -> raise lexError

        scan (str2lst input) // Scans the list of tokens from input string

    let readInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()

    // Grammar in BNF:
    // <expr> ::= <term>
    //  | <expr> + <term>
    //  | <expr> - <term>
    // <term> ::= <factor>
    //  | <term> * <factor>
    //  | <term> / <factor>
    // <factor> ::= <int> | (<expr>)
    // <int> ::= <digit> | <int><digit>
    // <digit> ::= 0|1|2|3|4|5|6|7|8|9

    // parser checks that the input token list conforms to the grammar
    let parser tokenList = 
        Console.WriteLine("Parsing: {0}", tokenList.ToString())
        let rec parseExpression tokens = 
            (parseTerm >> parseRestOfExpression) tokens         // >> is forward function composition operator: let inline (>>) parseRestOfExpression(parseTerm(tokens)) / parseRestOfExpression parseTerm tokens in F#
        and parseRestOfExpression tokens = 
            match tokens with
            | Add :: tail -> (parseTerm >> parseRestOfExpression) tail
            | Sub :: tail -> (parseTerm >> parseRestOfExpression) tail
            | _ -> tokens
        and parseTerm tokens = (parseFactor >> parseRestOfTerm) tokens
        and parseRestOfTerm tokens =
            match tokens with
            | Mul :: tail -> (parseFactor >> parseRestOfTerm) tail
            | Div :: tail -> (parseFactor >> parseRestOfTerm) tail
            | Rem :: tail -> (parseFactor >> parseRestOfTerm) tail
            | _ -> tokens
        and parseFactor tokens =
            match tokens with 
            | Num value :: tail -> tail
            | Cos :: tail -> parseFactor tail
            | Sin :: tail -> parseFactor tail
            | Tan :: tail -> parseFactor tail
            | Lpar :: tail -> match parseExpression tail with 
                              | Rpar :: tail -> tail
                              | _ -> raise parseError
            | _ -> raise parseError
        parseExpression tokenList

    let parseAndEval tokenList = 
        let rec evalExpression tokens = 
            (evalTerm >> evalRestOfExpression) tokenList
        and evalRestOfExpression (tokens, value) = 
            match tokens with
            | Add :: tail -> let (tokenRemainder, termVal) = evalTerm tail
                             evalRestOfExpression (tokenRemainder, value + termVal)
            | Sub :: tail -> let (tokenRemainder, termVal) = evalTerm tail
                             evalRestOfExpression (tokenRemainder, value - termVal)
            | _ -> (tokens, value)
        and evalTerm tokens = (evalFactor >> evalRestOfTerm) tokens
        and evalRestOfTerm (tokens, value) =
            match tokens with
            | Mul :: tail -> let (tokenRemainder, factorVal) = evalFactor tail
                             evalRestOfTerm (tokenRemainder, value * factorVal)
            | Div :: tail -> let (tokenRemainder, factorVal) = evalFactor tail
                             if factorVal = 0 then
                                raise (System.DivideByZeroException("Division by zero"))
                             else
                                evalRestOfTerm (tokenRemainder, value / factorVal)
            | Rem :: tail -> let (tokenRemainder, factorVal) = evalFactor tail
                             evalRestOfTerm (tokenRemainder, value % factorVal)
            | _ -> (tokens, value)
        and evalFactor tokens =
            match tokens with 
            | Num value :: tail -> (tail, value) // would turn [Num 9; Add; Num 1] into ([Add; Num 1], 9)
            | Cos value :: tail -> let (tokenRemainder, termVal) = evalFactor tail
                                   (tokenRemainder, Math.Cos termVal)
            | Sin value :: tail -> let (tokenRemainder, termVal) = evalFactor tail
                                   (tokenRemainder, Math.Sin termVal)
            | Tan value :: tail -> let (tokenRemainder, termVal) = evalFactor tail
                                   (tokenRemainder, Math.Tan termVal)
            | Lpar :: tail -> let (tokenRemainder, termVal) = evalExpression tail
                              match tokenRemainder with 
                              | Rpar :: tail -> (tail, termVal)
                              | _ -> raise parseError
            | _ -> raise parseError
        evalExpression tokenList

    let rec printTokenList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTokenList tail
                  
        | [] -> Console.Write("EOL\n")
                []


    [<EntryPoint>]
    let main argv  =
        Console.WriteLine("Simple Interpreter - Enter Code")
        let input:string = readInputString()
        let operationList = lexer input // Converts input to operation list e.g. "91 + 1" -> [Num 91; Add; Num 1]
        let sList = printTokenList operationList; // Prints the operation list e.g. > Num 91 Add Num 1 EOL
        let pList = printTokenList (parser operationList) // Prints the remaining list after parsing e.g. > EOL
        let Out = parseAndEval operationList
        Console.WriteLine("Result = {0}", snd Out)
        0
