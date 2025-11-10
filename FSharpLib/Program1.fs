// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

namespace FSharpLib

module Numera = 

    open System

    // Variable declaration and symbol table

    type ValType =
        | IntType
        | FloatType
        | BoolType
        | StringType

    type Val =
        | ValInt of int
        | ValFloat of float
        | ValBool of bool
        | ValString of string

    type Symbol = { 
        valType: ValType
        value: Val option 
        }

    type SymTable = Map<string, Symbol>
    let emptyTable : SymTable = Map.empty
    let symbolTable = ref emptyTable
    let declareVariable name varType value =
        if Map.containsKey name !symbolTable then // !symbolTable dereferences the table
            failwithf "Variable %s has already been defined" name
        else 
            // use name as map key, {valType; value} as map value, and add it to symbolTable
            symbolTable := Map.add name {valType = varType; value = Some value} !symbolTable

    let inferVariableType (value:Val) : ValType =
        match value with
        | ValInt _ -> IntType
        | ValFloat _ -> FloatType
        | ValBool _ -> BoolType
        | ValString _ -> StringType

    // converts integer to float for variable assignment / symbol table usage
    let intToFloatConvert (varType: ValType) (value:Val) : Val =
        match (varType, value) with 
        | FloatType, ValInt i -> ValFloat (float i)
        | _ -> value

    // convert number to float for evaluation
    let valToFloat (value: Val) : float =
        match value with
        | ValInt i -> float i
        | ValFloat f -> f
        | ValBool b -> if b then 1.0 else 0.0
        | ValString _ -> raise (System.Exception(sprintf "String %A cannot be used in numeric context." value)) 
        // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/exception-handling/the-raise-function
        // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting

    // convert float result to chosen ValType
    let floatToValType (declaredType: ValType) (value: float) : Val =
        match declaredType with
        | FloatType -> ValFloat value
        | IntType -> 
            if value = floor value then
                ValInt (int value)
            else
                raise (System.Exception("Lexer error"))
        | _ -> raise (System.Exception("floatToValType called for a type which is not numeric."))

    let floatIntTypeCheck (varType: ValType) (v:Val) : Val =
        let finalVal =
            match v with
                | ValInt _ -> intToFloatConvert varType v
                | _ -> v
        match (varType, finalVal) with
        | FloatType, ValFloat f -> ValFloat f
        | IntType, ValInt i -> ValInt i
        | BoolType, ValBool b -> ValBool b
        | StringType, ValString s -> ValString s
        | _ -> raise (System.Exception(sprintf "Type mismatch: declared %A but got %A" varType v))


    // terminal, parser and evaluator

    //<statement> ::= 'let' <type> <identifier> '=' <expr> ';' | <expr>
    //<expr> ::= <term>
        //| <expr> + <term>
        //| <expr> - <term>
    //<term> ::= <factor>
        //| <term> * <factor>
        //| <term> / <factor>
    //<power> ::= <factor>^<power>
    //<factor> ::= <int> | (<expr>)
    //<int> ::= <digit> | <int><digit>
    //<digit> ::= 0|1|2|3|4|5|6|7|8|9

    type terminal = 
        Add | Sub | Mul | Div | Rem | Power | Lpar | Rpar
        | NumInt of int
        | NumFloat of float
        | StringLit of string
        | Identifier of string
        | Let
        | TypeInt | TypeFloat | TypeBool
        | Equals | Semicolon

    let varDeclarationRHS 
        (declarationType: ValType) 
        (tokens: list<terminal>) 
        (evalExpression : list<terminal> -> (list<terminal> * float)) 
        : (Val * list<terminal>) =

        match tokens with
        | Identifier "true" :: tail ->
            if declarationType <> BoolType then raise (System.Exception("Cannot assign boolean to non-bool"))
            (ValBool true, tail)
        | Identifier "false" :: tail ->
            if declarationType <> BoolType then raise (System.Exception("Cannot assign boolean to non-bool"))
            (ValBool false, tail)
        | Identifier name :: tail ->
            if Map.containsKey name !symbolTable then
                let variableFound = (!symbolTable).[name].value
                match variableFound with
                | None -> raise (System.Exception("Variable used before initialisation"))
                | Some value ->
                    let (afterExpressionTail, floatValue) = evalExpression tokens
                    match declarationType, value with
                    | BoolType, ValBool foundBoolean -> (ValBool foundBoolean, tail)
                    | BoolType, _ -> raise (System.Exception("Cannot assign non-bool to bool"))
                    | (FloatType | IntType), _ ->
                        let targetValue = floatToValType declarationType floatValue
                        let confirmed = floatIntTypeCheck declarationType targetValue
                        (confirmed, afterExpressionTail)
                    | StringType, ValString foundString -> (ValString foundString, tail)
                    | _, _ -> raise (System.Exception(sprintf "Type mismatch: declared %A but got %A" declarationType value))
            else
                raise (System.Exception("Undefined variable usage attempted"))
        | _ -> 
            let (afterExpressionTokens, floatVal) = evalExpression tokens
            match declarationType with
            | BoolType -> raise (System.Exception("Cannot assign numeric expression to bool"))
            | (FloatType | IntType) ->
                let converted = floatToValType declarationType floatVal
                let confirmed = floatIntTypeCheck declarationType converted
                (confirmed, afterExpressionTokens)
            | StringType ->
                raise (System.Exception("String declaration from expression not possible"))

    let add(x:int, y:int) : int = x + y

    let str2lst s = [for c in s -> c] // Converts the input string to list of characters
    let isBlank c = System.Char.IsWhiteSpace c
    let isDigit c = System.Char.IsDigit c
    let isLetter c = System.Char.IsLetter c
    let lexError = System.Exception("Lexer error")
    let charToDigit (c:char) = (int)((int)c - (int)'0')
    let parseError = System.Exception("Parser error")

    let rec scanDecimal(chars: char list) (value: string) : (char list * string) =
        match chars with
        | c::tail when isDigit c -> scanDecimal tail (value + string c)
        | _ -> (chars, value)

    let rec scanNumber(chars: char list) (value: string) : (char list * string) = 
        match chars with
        | c :: tail when isDigit c -> 
            scanNumber tail (value + string c)
        | '.' :: tail -> 
                        let (rest, decimal) = scanDecimal tail  ""
                        if decimal = "" then
                            raise lexError
                        else
                            (rest, value + "." + decimal)
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
            | '('::tail -> Lpar :: scan tail
            | ')'::tail -> Rpar :: scan tail
            | '='::tail -> Equals :: scan tail
            | ';'::tail -> Semicolon :: scan tail
            | c :: tail when isBlank c -> scan tail
            | c :: tail when isDigit c ->   let (remaining, valueStr) = scanNumber tail (string c)
                                            if valueStr.Contains(".") then
                                                let value = Double.Parse(valueStr)
                                                NumFloat value :: scan remaining
                                            else
                                                let value = Int32.Parse(valueStr)
                                                NumInt value :: scan remaining
            | c :: tail when isLetter c -> let (remaining, word) = scanIdentifier(tail, string c)
                                           match word.ToLower() with
                                           | "let" -> Let :: scan remaining
                                           | "int" -> TypeInt :: scan remaining
                                           | "float" -> TypeFloat :: scan remaining
                                           | "bool" -> TypeBool :: scan remaining
                                           | "sin" -> Identifier "sin" :: scan remaining
                                           | "cos" -> Identifier "cos" :: scan remaining
                                           | "tan" -> Identifier "tan" :: scan remaining
                                           | "true" -> Identifier "true" :: scan remaining
                                           | "false" -> Identifier "false" :: scan remaining
                                           | other -> Identifier other :: scan remaining
            | _ -> raise lexError

        scan (str2lst input)

    let readInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()

    let parser tokenList =
        Console.WriteLine("Parsing: {0}", tokenList.ToString())

        let rec parseStatement tokens =
            match tokens with
            | Let :: TypeInt :: Identifier name :: Equals :: tail
            | Let :: TypeFloat :: Identifier name :: Equals :: tail
            | Let :: TypeBool :: Identifier name :: Equals :: tail ->
                let rest = parseExpression tail
                match rest with
                | Semicolon :: tail2 -> tail2
                | _ -> raise parseError
            | _ -> parseExpression tokens

        and parseExpression tokens =
            let termRest = parseTerm tokens
            parseRestOfExpression termRest

        and parseRestOfExpression tokens =
            match tokens with
            | Add :: tail ->
                let termRest = parseTerm tail
                parseRestOfExpression termRest
            | Sub :: tail ->
                let termRest = parseTerm tail
                parseRestOfExpression termRest
            | _ -> tokens

        and parseTerm tokens =
            let powerRest = parsePower tokens
            parseRestOfTerm powerRest

        and parseRestOfTerm tokens =
            match tokens with
            | Mul :: tail ->
                let powerRest = parsePower tail
                parseRestOfTerm powerRest
            | Div :: tail ->
                let powerRest = parsePower tail
                parseRestOfTerm powerRest
            | Rem :: tail ->
                let powerRest = parsePower tail
                parseRestOfTerm powerRest
            | _ -> tokens

        and parsePower tokens =
            let factorRest = parseFactor tokens
            match factorRest with
            | Power :: tail ->
                let powerRest = parsePower tail
                powerRest
            | _ -> factorRest

        and parseFactor tokens =
            match tokens with
            | NumInt _ :: tail -> tail
            | NumFloat _ :: tail -> tail
            | Identifier name :: tail ->
                if name = "sin" || name = "cos" || name = "tan" then
                    let tailRest = parseFactor tail
                    tailRest
                else
                    tail
            | Lpar :: tail ->
                let exprRest = parseExpression tail
                match exprRest with
                | Rpar :: tail2 -> tail2
                | _ -> raise parseError
            | _ -> raise parseError

        parseStatement tokenList


    let parseAndEval tokenList = 
        let floatRem (a:float) (b:float) = a - (floor (a / b) * b)

        let rec evalStatement tokens =
            match tokens with
            | Let :: TypeInt :: Identifier name :: Equals :: tail
            | Let :: TypeFloat :: Identifier name :: Equals :: tail
            | Let :: TypeBool :: Identifier name :: Equals :: tail ->
                let declaredType = 
                    match tokens.[1] with
                    | TypeInt -> IntType
                    | TypeFloat -> FloatType
                    | TypeBool -> BoolType
                    | _ -> raise parseError
                    
                let (valueVal, tokenRemainer) = varDeclarationRHS declaredType tail evalExpression

                printfn "Token remainder: %A" tokenRemainer
                
                match tokenRemainer with
                | Semicolon :: restAfterSemi ->
                    declareVariable name declaredType valueVal
                    (restAfterSemi, valToFloat valueVal)
                | _ -> raise (System.Exception("Missing semicolon in declaration"))
            | _ -> evalExpression tokens
        and evalExpression tokens = 
            (evalTerm >> evalRestOfExpression) tokens
        and evalRestOfExpression (tokens, value) = 
            match tokens with
            | Add :: tail -> let (tokenRemainder, termVal) = evalTerm tail
                             evalRestOfExpression (tokenRemainder, value + termVal)
            | Sub :: tail -> let (tokenRemainder, termVal) = evalTerm tail
                             evalRestOfExpression (tokenRemainder, value - termVal)
            | _ -> (tokens, value)
        and evalTerm tokens = (evalPower >> evalRestOfTerm) tokens
        and evalRestOfTerm (tokens, value) =
            match tokens with
            | Mul :: tail -> let (tokenRemainder, factorVal) = evalFactor tail
                             evalRestOfTerm (tokenRemainder, value * factorVal)
            | Div :: tail -> let (tokenRemainder, factorVal) = evalFactor tail
                             if factorVal = 0.0 then
                                raise (System.Exception("Division by zero"))
                             else
                                evalRestOfTerm (tokenRemainder, value / factorVal)
            | Rem :: tail -> let (tokenRemainder, factorVal) = evalFactor tail
                             evalRestOfTerm (tokenRemainder, floatRem value factorVal)
            | _ -> (tokens, value)
        and evalPower tokens =
            let (remainderAfterBase, baseVal) = evalFactor tokens
            match remainderAfterBase with
            | Power :: tail -> let (tokenRemainder, exponentVal) = evalPower tail
                               (tokenRemainder, baseVal**exponentVal)
            | _ -> (remainderAfterBase, baseVal)
        and evalFactor tokens =
            printfn "Tokens : %A" tokens
            match tokens with 
            | NumInt value :: tail -> (tail, float value)
            | NumFloat value :: tail -> (tail, value)
            | Identifier name :: tail ->
                // function application: sin/cos/tan(expect a following factor)
                if name = "sin" || name = "cos" || name = "tan" then
                    let (rest, v) = evalFactor tail
                    match name with
                    | "sin" -> (rest, Math.Sin v)
                    | "cos" -> (rest, Math.Cos v)
                    | "tan" -> (rest, Math.Tan v)
                    | _ -> failwith "unreachable"
                else
                    // variable lookup (return its numeric value and consume just the identifier)
                    if Map.containsKey name !symbolTable then
                        let variableFound = (!symbolTable).[name].value
                        match variableFound with
                        | None -> raise (System.Exception("Variable used before initialisation"))
                        | Some value -> (tail, valToFloat value)
                    else
                        raise parseError
            | Lpar :: tail -> let (tokenRemainder, termVal) = evalExpression tail
                              match tokenRemainder with 
                              | Rpar :: tail -> (tail, termVal)
                              | _ -> raise parseError
            | _ -> raise parseError
        evalStatement tokenList

    let rec printTokenList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTokenList tail
                  
        | [] -> Console.Write("EOL\n")
                []

    let rec repeat() =
        let input = readInputString()
        if input.ToLower() = "exit" then
            Console.WriteLine("Exiting interpreter.")
        else
            let tokens = lexer input
            let (_, result) = parseAndEval tokens 
            Console.WriteLine("Result = {0}", result)
            repeat()

    let main argv  =
        Console.WriteLine("Simple Interpreter - Enter Code (type 'exit' to quit)")
        repeat()
        0