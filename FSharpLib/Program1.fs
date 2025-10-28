// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

namespace FSharpLib

module Numera = 

    open System

    type terminal = 
        Add | Sub | Mul | Div | Rem | Power | Lpar | Rpar 
        | Num of float
        | Identifier of string

    let add(x:int, y:int) : int = x + y

    let str2lst s = [for c in s -> c] // Converts the input string to list of characters
    let isBlank c = System.Char.IsWhiteSpace c
    let isDigit c = System.Char.IsDigit c
    let isLetter c = System.Char.IsLetter c
    let lexError = System.Exception("Lexer error")
    let charToDigit (c:char) = (int)((int)c - (int)'0')
    let parseError = System.Exception("Parser error")

    let rec scanDecimal(chars, acc:string) =
        match chars with
        | c::tail when isDigit c -> scanDecimal(tail, acc + string c)
        | _ -> (chars, acc)

    let rec scanNumber(chars, value:string) = 
        Console.Write("{0}, {1}", chars, value)
        match chars with
        | c :: tail when isDigit c -> scanNumber(tail, value + string c)
        | '.' :: tail -> let (rest, decimal) = scanDecimal(tail, "") in (rest, value + "." + decimal)
        | _ -> (chars, value)

    let rec scanIdentifier(remaining, identStr) =
        match remaining with
        c :: tail when isLetter c -> scanIdentifier(tail, identStr + string c)
        | _ -> (remaining, identStr)

    let lexer input = 
        let rec scan input =
            match input with
            | [] -> []
            | '+'::tail -> Add :: scan tail
            | '-'::tail -> Sub :: scan tail
            | '*'::tail -> Mul :: scan tail
            | '/'::tail -> Div :: scan tail
            | '^'::tail -> Power :: scan tail
            | '%'::tail -> Rem :: scan tail
            | '('::tail -> Lpar:: scan tail
            | ')'::tail -> Rpar:: scan tail
            | c :: tail when isBlank c -> scan tail
            | c :: tail when isDigit c -> let (remaining, valueStr) = scanNumber(tail, string c)
                                          let value = Double.Parse(valueStr)
                                          Num value :: scan remaining
            | c :: tail when isLetter c -> let (remaining, word) = scanIdentifier(tail, string c)
                                           match word.ToLower() with
                                           | "sin" -> Identifier "sin" :: scan remaining
                                           | "cos" -> Identifier "cos" :: scan remaining
                                           | "tan" -> Identifier "tan" :: scan remaining
                                           | other -> Identifier other :: scan remaining

            | _ -> raise lexError

        scan (str2lst input)

    let readInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()

    let parser tokenList = 
        Console.WriteLine("Parsing: {0}", tokenList.ToString())
        let rec parseExpression tokens = 
            (parseTerm >> parseRestOfExpression) tokens
        and parseRestOfExpression tokens = 
            match tokens with
            | Add :: tail -> (parseTerm >> parseRestOfExpression) tail
            | Sub :: tail -> (parseTerm >> parseRestOfExpression) tail
            | _ -> tokens
        and parseTerm tokens = (parseFactor >> parseRestOfTerm) tokens
        and parseRestOfTerm tokens =
            match tokens with
            | Power :: tail -> (parseFactor >> parseRestOfTerm) tail
            | Mul :: tail -> (parseFactor >> parseRestOfTerm) tail
            | Div :: tail -> (parseFactor >> parseRestOfTerm) tail
            | Rem :: tail -> (parseFactor >> parseRestOfTerm) tail
            | _ -> tokens
        and parseFactor tokens =
            match tokens with 
            | Num value :: tail -> tail
            | Identifier _ :: tail -> parseFactor tail
            | Lpar :: tail -> match parseExpression tail with 
                              | Rpar :: tail -> tail
                              | _ -> raise parseError
            | _ -> raise parseError
        parseExpression tokenList

    let parseAndEval tokenList = 
        let floatRem (a:float) (b:float) = a - (floor (a / b) * b)

        let rec evalExpression tokens = 
            (evalTerm >> evalRestOfExpression) tokens
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
            | Power :: tail -> let (tokenRemainder, factorVal) = evalFactor tail
                               evalRestOfTerm (tokenRemainder, value**factorVal)
            | Mul :: tail -> let (tokenRemainder, factorVal) = evalFactor tail
                             evalRestOfTerm (tokenRemainder, value * factorVal)
            | Div :: tail -> let (tokenRemainder, factorVal) = evalFactor tail
                             if factorVal = 0.0 then
                                raise (System.DivideByZeroException("Division by zero"))
                             else
                                evalRestOfTerm (tokenRemainder, value / factorVal)
            | Rem :: tail -> let (tokenRemainder, factorVal) = evalFactor tail
                             evalRestOfTerm (tokenRemainder, floatRem value factorVal)
            | _ -> (tokens, value)
        and evalFactor tokens =
            match tokens with 
            | Num value :: tail -> (tail, value)
            | Identifier name :: tail ->
                let (rest, v) = evalFactor tail
                match name with
                | "sin" -> (rest, Math.Sin v)
                | "cos" -> (rest, Math.Cos v)
                | "tan" -> (rest, Math.Tan v)
                | _ -> raise parseError
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
        let operationList = lexer input
        let sList = printTokenList operationList;
        let pList = printTokenList (parser operationList)
        let Out = parseAndEval operationList
        Console.WriteLine("Result = {0}", snd Out)
        0