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

    let valAdd (value1:Val) (value2:Val) : Val =
        match (value1, value2) with
        | (ValInt i, ValInt j) -> ValInt(i + j)
        | (ValInt i, ValFloat j) -> ValFloat (float i + j)
        | (ValFloat i, ValInt j) -> ValFloat (i + float j)
        | (ValFloat i, ValFloat j) -> ValFloat(i + j)
        | _ -> raise (System.Exception("Addition error"))

    let valSub (value1:Val) (value2:Val) : Val =
        match (value1, value2) with
        | (ValInt i, ValInt j) -> ValInt(i - j)
        | (ValInt i, ValFloat j) -> ValFloat (float i - j)
        | (ValFloat i, ValInt j) -> ValFloat (i - float j)
        | (ValFloat i, ValFloat j) -> ValFloat(i - j)
        | _ -> raise (System.Exception("Subtraction error"))

    let valMult (value1:Val) (value2:Val) : Val =
        match (value1, value2) with
        | (ValInt i, ValInt j) -> ValInt(i * j)
        | (ValInt i, ValFloat j) -> ValFloat (float i * j)
        | (ValFloat i, ValInt j) -> ValFloat (i * float j)
        | (ValFloat i, ValFloat j) -> ValFloat(i * j)
        | _ -> raise (System.Exception("Multiplication error"))

    let valDiv (value1:Val) (value2:Val) : Val =
        match (value1, value2) with
        | (ValInt 0, _)
        | (ValFloat 0.0, _)
        | (_, ValInt 0)
        | (_, ValFloat 0.0) -> raise (System.Exception("Division by 0 error"))
        | (ValInt i, ValInt j) -> ValInt (i / j)
        | (ValInt i, ValFloat j) -> ValFloat (float i / j)
        | (ValFloat i, ValInt j) -> ValFloat (i / float j)
        | (ValFloat i, ValFloat j) -> ValFloat (i / j)
        | _ -> raise (System.Exception("Division error"))


    let valRem (value1:Val) (value2:Val) : Val =
        match (value1, value2) with
        | ValInt 0, _
        | ValFloat 0.0, _ -> ValInt 0
        | _, ValInt 0
        | _, ValFloat 0.0 -> ValInt 1
        | (ValInt i, ValInt j) -> ValInt(i % j)
        | (ValInt i, ValFloat j) -> ValFloat (float i % j)
        | (ValFloat i, ValInt j) -> ValFloat (i % float j)
        | (ValFloat i, ValFloat j) -> ValFloat(i % j)
        | _ -> raise (System.Exception("Remainder error"))

    let valPow (value1:Val) (value2:Val) : Val =
        match (value1, value2) with
        | (ValInt i, ValInt j) -> ValInt(pown i j)
        | _ -> raise (System.Exception("Exponential error"))

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
    //<graph> ::= 'graph' <identifier> '=' <expr> [ ',' '(' <num> ',' <num> ')' ] ';'
    //<type> ::= <int> | <float> | <bool>
    //<expr> ::= <term>
        //| <expr> + <term>
        //| <expr> - <term>
    //<term> ::= <power>
        //| <term> * <power>
        //| <term> / <power>
    //<unary> ::= -<unary> | <power>
    //<power> ::= <factor> ^ <power> | <factor>
    //<factor> ::= <number>
        //| <identifier>
        //| <function> <factor>
        //| "(" <expr> ")"
        //| <expr>
    //<number> ::= <int> | <float>
    //<float> ::= <int> . <int>
    //<function> ::= sin | cos | tan
    //<int> ::= <digit> | <int><digit>
    //<identifier>   ::= letter { letter | digit | "_" }
    //<digit> ::= 0|1|2|3|4|5|6|7|8|9
    //<boolean> ::= true | false

    type Numeric = ASTNumInt of int | ASTNumFloat of float
    type binaryOp = ASTAdd | ASTSub | ASTMul | ASTDiv | ASTRem | ASTPower
    type unaryOp = Neg
    type ASTsyntax =
      | Number of float
      | ASTIdentifier of string
      | Unary of unaryOp * ASTsyntax
      | Binary of binaryOp * ASTsyntax * ASTsyntax
      | FuncCall of string * ASTsyntax

    type terminal = 
        Add | Sub | Mul | Div | Rem | Power | Lpar | Rpar
        | NumInt of int
        | NumFloat of float
        | StringLit of string
        | Identifier of string
        | Let 
        | TypeInt | TypeFloat | TypeBool
        | Equals | Semicolon | Comma
        | Graph
        | GraphDif
        | GraphInt
        | FindRoot
        

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
            let tokensForEval =
                match declarationType with
                | FloatType ->                            // if declared as float,
                    tokens
                    |> List.map (function                 // maps over tokens and converts them to floats (https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/lists)
                        | NumInt n -> NumFloat (float n)
                        | other    -> other)
                | _ -> tokens

            let (remaining, result) = evalExpression tokensForEval
            let value = floatToValType declarationType result

            (value, remaining)

    let add(x:int, y:int) : int = x + y

    let str2lst s = [for c in s -> c] // Converts the input string to list of characters
    let isBlank c = System.Char.IsWhiteSpace c
    let isDigit c = System.Char.IsDigit c
    let isLetter c = System.Char.IsLetter c
    let lexError = System.Exception("Lexer error")
    let charToDigit (c:char) = (int)((int)c - (int)'0')
    let parseError = System.Exception("Parser error")

    // identifies whether value is a float
    let isFloat (value) =
        let s = string value
        let mutable d = 0.0
        let ok = Double.TryParse(s, &d)
        ok && s.Contains(".")

    // scans decimal number to understand it
    let rec scanDecimal(chars: char list) (value: string) : (char list * string) =
        match chars with
        | c::tail when isDigit c -> scanDecimal tail (value + string c)
        | _ -> (chars, value)

    // scans number (integer or decimal) to understand it
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

    // scans identifier (written text that is not a number) to understand it
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
            | ','::tail -> Comma :: scan tail
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
                                           | "graph" -> Graph :: scan remaining
                                           | "graphdif" -> GraphDif :: scan remaining
                                           | "graphint" -> GraphInt :: scan remaining
                                           | "findroot" -> FindRoot :: scan remaining
                                           | "x" -> Identifier "x" :: scan remaining
                                           | "y" -> Identifier "y" :: scan remaining
                                           | "exp" -> Identifier "exp" :: scan remaining
                                           | "log" -> Identifier "log" :: scan remaining
                                           | other -> Identifier other :: scan remaining
            | _ -> raise lexError

        scan (str2lst input)

    let parser tokenList =
        Console.WriteLine("Parsing: {0}", tokenList.ToString())

        let rec parseStatement tokens =
            match tokens with
            | Let :: TypeInt :: Identifier name :: Equals :: tail
            | Let :: TypeFloat :: Identifier name :: Equals :: tail
            | Let :: TypeBool :: Identifier name :: Equals :: tail ->
                let (exprAST, rest) = parseGraphStatement tail
                match rest with
                | Semicolon :: tail2 -> tail2
                | _ -> raise parseError
            | _ -> 
                let (exprAST, restTokens) = parseGraphStatement tokens
                restTokens

        // graph statements are all parsed te same way
        and parseGraphStatement tokens =
            match tokens with
            | Graph :: Identifier name :: Equals :: tail
            | GraphDif :: Identifier name :: Equals :: tail
            | GraphInt :: Identifier name :: Equals :: tail
            | FindRoot :: Identifier name :: Equals :: tail ->
                parseExpression tail
            | _ -> parseExpression tokens

        and parseExpression tokens =
            let (leftAST, termRest) = parseTerm tokens
            parseRestOfExpression leftAST termRest

        and parseRestOfExpression leftAST tokens =
            match tokens with
            | Add :: tail ->
                let (rightAST, termRest) = parseTerm tail
                let combinedAST = Binary(ASTAdd, leftAST, rightAST)
                parseRestOfExpression combinedAST termRest
            | Sub :: tail ->
                let (rightAST, termRest) = parseTerm tail
                let combinedAST = Binary(ASTSub, leftAST, rightAST)
                parseRestOfExpression combinedAST termRest
            | _ -> (leftAST, tokens)

        and parseTerm tokens =
            let (leftAST, unaryRest) = parseUnary tokens
            parseRestOfTerm leftAST unaryRest

        and parseRestOfTerm leftAST tokens =
            match tokens with
            | Mul :: tail ->
                let (rightAST, powerRest) = parsePower tail
                let combinedAST = Binary(ASTMul, leftAST, rightAST)
                parseRestOfTerm combinedAST powerRest
            | Div :: tail ->
                let (rightAST, powerRest) = parsePower tail
                let combinedAST = Binary(ASTDiv, leftAST, rightAST)
                parseRestOfTerm combinedAST powerRest
            | Rem :: tail ->
                let (rightAST, powerRest) = parsePower tail
                let combinedAST = Binary(ASTRem, leftAST, rightAST)
                parseRestOfTerm combinedAST powerRest
            | _ -> (leftAST, tokens)

         and parseUnary tokens =
            match tokens with
            | Sub :: tail ->
                let (innerAST, unaryRest) = parseUnary tail
                (Unary(Neg, innerAST), unaryRest)
            | _ -> parsePower tokens

        and parsePower tokens =
            let (baseAST, factorRest) = parseFactor tokens
            match factorRest with
            | Power :: tail ->
                let (expAST, powerRest) = parsePower tail
                (Binary(ASTPower, baseAST, expAST), powerRest)
            | _ -> (baseAST, factorRest)

        and parseFactor tokens =
            match tokens with
            | NumInt n :: tail -> (Number (float n), tail)
            | NumFloat n :: tail -> (Number (float n), tail)
            | Identifier name :: tail ->
                if name = "sin" || name = "cos" || name = "tan" then
                    let (argAST, tailRest) = parseFactor tail
                    (FuncCall(name, argAST), tailRest)
                else
                    (ASTIdentifier name, tail)
            | Lpar :: tail ->
                let (innerAST, exprRest) = parseExpression tail
                match exprRest with
                | Rpar :: tail2 -> (innerAST, exprRest)
                | _ -> raise parseError
            | _ -> raise parseError

        parseStatement tokenList


    // used to evaluate statements
    let parseAndEval tokenList = 

        // pushes temporary binding for variable during graph evaluation
        let pushTempBinding (name:string) (v:Val) =
            let old = Map.tryFind name !symbolTable
            let temp = match v with | ValInt _ -> IntType | ValFloat _ -> FloatType | ValBool _ -> BoolType | ValString _ -> StringType
            symbolTable := Map.add name { valType = temp; value = Some v } !symbolTable
            old

        // pops binding after graph evaluation
        let popRestoreBinding (name:string) (oldOpt: Symbol option) =
            match oldOpt with
            | None -> symbolTable := Map.remove name !symbolTable
            | Some s -> symbolTable := Map.add name s !symbolTable

        // converts a Val to a string
        let valToString (v:Val) =
            match v with
            | ValInt i -> sprintf "%d" i
            | ValFloat f -> sprintf "%g" f
            | ValBool b -> if b then "true" else "false"
            | ValString s -> s

        // formats list of pairs - used for graph output
        let formatPairs (pairs:(Val * Val) list) =
            let elems = pairs |> List.map (fun (x,y) -> sprintf "(%s,%s)" (valToString x) (valToString y))
            "[" + (String.Join(",", elems)) + "]"

        // split graph tokens into equation and graph range
        let rec splitGraphTokens (tokens: terminal list)
            : (terminal list * float * float * terminal list) =

            let rec loop accumulatedTokens bracketDepth remainingTokens =
                match remainingTokens with
                | [] ->
                    (List.rev accumulatedTokens, 10.0, 10.0, [])

                | Semicolon :: tail ->
                    (List.rev accumulatedTokens, 10.0, 10.0, Semicolon :: tail)

                | Comma :: Lpar :: xToken :: Comma :: yToken :: Rpar :: tail
                  when bracketDepth = 0 ->
                    let parseNumber token =
                        match token with
                        | NumInt n -> float n
                        | NumFloat f -> f
                        | _ -> raise (System.Exception("Expected numeric literal in graph range"))

                    let xRange = parseNumber xToken
                    let yRange = parseNumber yToken

                    (List.rev accumulatedTokens, xRange, yRange, tail)
                    
                | headToken :: tailTokens ->
                    let newBracketDepth =
                        match headToken with
                        | Lpar -> bracketDepth + 1
                        | Rpar -> bracketDepth - 1
                        | _ -> bracketDepth

                    loop (headToken :: accumulatedTokens) newBracketDepth tailTokens

            loop [] 0 tokens


        // evaluates graph
        and varGraphRHS (varName:string) (exprTokens:terminal list) (xMax:float) (yMax:float) : (Val * Val) list =
            let numOfPoints = 100
            let rec sample i acc =
                if i >= numOfPoints then List.rev acc
                else
                    let computedX = -xMax + float (i + 1) * (2.0 * xMax) / float (numOfPoints + 1)
                    let xValSymb =
                        let j = int computedX
                        if float j = computedX then ValInt j else ValFloat computedX

                    let prevVar = pushTempBinding varName xValSymb
                    let prevX = pushTempBinding "x" xValSymb
                    try
                        try
                            let (remaining, yVal) = evalExpressionVal exprTokens
                            let yValFloat = valToFloat yVal
                            if yValFloat > -yMax && yValFloat < yMax then
                                sample (i + 1) ((xValSymb, yVal) :: acc)
                            else
                                sample (i + 1) acc
                        with ex ->
                            Console.WriteLine("Skipping {0}={1}: {2}", varName, computedX, ex.Message)
                            sample (i + 1) acc
                    finally
                        popRestoreBinding "x" prevX
                        popRestoreBinding varName prevVar
            sample 0 []

        // differentiates a graph equation
        and varGraphDifRHS (varName:string) (exprTokens:terminal list) (xMax:float) (yMax:float) : (Val * Val) list =
            let numOfPoints = 100
            let delta = 1e-6
            let rec sample i acc =
                if i >= numOfPoints then List.rev acc
                else
                    let x = -xMax + float (i + 1) * (2.0 * xMax) / float (numOfPoints + 1)
                    let xValSymb =
                        let j = int x
                        if float j = x then ValInt j else ValFloat x

                    let valueToBind =
                        if float (int (x + delta)) = (x + delta) then
                            ValInt (int (x + delta))
                        else
                            ValFloat (x + delta)
                    let prevPlusVar = pushTempBinding varName valueToBind
                    let prevPlusX   = pushTempBinding "x" valueToBind
                    let yPlus =
                        try
                            let (_, v) = evalExpressionVal exprTokens
                            valToFloat v
                        finally
                            popRestoreBinding "x" prevPlusX
                            popRestoreBinding varName prevPlusVar
                    let prevMinusVar = pushTempBinding varName (if float (int (x - delta)) = x - delta then ValInt (int (x - delta)) else ValFloat (x - delta))
                    let prevMinusX = pushTempBinding "x" (if float (int (x - delta)) = x - delta then ValInt (int (x - delta)) else ValFloat (x - delta))
                    let yMinus =
                        try
                            let (_, evalVal) = evalExpressionVal exprTokens
                            valToFloat evalVal
                        finally
                            popRestoreBinding "x" prevMinusX
                            popRestoreBinding varName prevMinusVar
                    let deriv = (yPlus - yMinus) / (2.0 * delta)
                    let yVal =
                        if deriv = floor deriv then ValInt (int deriv) else ValFloat deriv
                    if valToFloat yVal > -yMax && valToFloat yVal < yMax then
                        sample (i + 1) ((xValSymb, yVal) :: acc)
                    else
                        sample (i + 1) acc
            sample 0 []

        and varGraphIntRHS (varName:string) (exprTokens:terminal list) (xmin:float) (xMax:float) (yMax:float) : (Val * Val) list =
            let numOfPoints = 100
            let nSub = 200
            let makeValFromFloat (t:float) =
                let rounded = System.Math.Round(t)
                if System.Math.Abs(t - rounded) < 1e-12 then ValInt (int rounded)
                else ValFloat t

            let safeNSub = max 1 nSub
            let rec sample i acc =
                if i >= numOfPoints then List.rev acc
                else
                    let x = xmin + float (i + 1) * ( (xMax - xmin) / float (numOfPoints + 1) )
                    let xValSymb = makeValFromFloat x

                    let lowerLimit = xmin
                    let upperLimit = x
                    let dx = (upperLimit - lowerLimit) / float safeNSub

                    let rec integrate stepIndex accInt =
                        if stepIndex > safeNSub then accInt
                        else
                            let t = lowerLimit + float stepIndex * dx
                            let prevVar = pushTempBinding varName (makeValFromFloat t)
                            let prevX   = pushTempBinding "x" (makeValFromFloat t)
                            let fval =
                                try
                                    let (_, v) = evalExpressionVal exprTokens
                                    valToFloat v
                                finally
                                    popRestoreBinding "x" prevX
                                    popRestoreBinding varName prevVar
                            let weight = if stepIndex = 0 || stepIndex = safeNSub then 0.5 else 1.0
                            integrate (stepIndex + 1) (accInt + weight * fval)

                    let integral = dx * integrate 0 0.0
                    let yVal = if integral = floor integral then ValInt (int integral) else ValFloat integral

                    if valToFloat yVal > -yMax && valToFloat yVal < yMax then
                        sample (i + 1) ((xValSymb, yVal) :: acc)
                    else
                        sample (i + 1) acc

            sample 0 []

        and evalStatement tokens =
            match tokens with
            | Graph :: Identifier name :: Equals :: tail ->
                let (exprTokens, xMax, yMax, remainder) = splitGraphTokens tail
                let restAfter =
                    match remainder with
                    | Semicolon :: rest -> rest
                    | _ -> remainder
                let pairs = varGraphRHS name exprTokens xMax yMax
                Console.WriteLine("{0}", formatPairs pairs)
                (restAfter, 0.0)
            | GraphDif :: Identifier name :: Equals :: tail ->
                let (exprTokens, xMax, yMax, remainder) = splitGraphTokens tail
                let restAfter = 
                    match remainder with
                    | Semicolon :: rest -> rest
                    | _ -> remainder
                let pairs = varGraphDifRHS name exprTokens xMax yMax
                Console.WriteLine("{0}", formatPairs pairs)
                (restAfter, 0.0)
            | GraphInt :: Identifier name :: Equals :: tail ->
                // splitGraphTokens currently returns (exprTokens, xMax, yMax, remainder)
                let (exprTokens, xMax, yMax, remainder) = splitGraphTokens tail
                let restAfter =
                    match remainder with
                    | Semicolon :: rest -> rest
                    | _ -> remainder

                // Provide xmin first (here we use symmetric range -xMax..xMax to preserve old behaviour)
                let xmin = -xMax
                let pairs = varGraphIntRHS name exprTokens xmin xMax yMax

                Console.WriteLine("{0}", formatPairs pairs)
                (restAfter, 0.0)
            | Let :: TypeInt :: Identifier name :: Equals :: tail
            | Let :: TypeFloat :: Identifier name :: Equals :: tail
            | Let :: TypeBool :: Identifier name :: Equals :: tail ->
                let declaredType = 
                    match tokens.[1] with
                    | TypeInt -> IntType
                    | TypeFloat -> FloatType
                    | TypeBool -> BoolType
                    | _ -> raise parseError
                
                let (valueVal, tokenRemainer) = varDeclarationRHS declaredType tail evalExpressionFloat

                printfn "Token remainder: %A" tokenRemainer
            
                match tokenRemainer with
                | Semicolon :: restAfterSemi ->
                    declareVariable name declaredType valueVal
                    (restAfterSemi, valToFloat valueVal)
                | _ -> raise (System.Exception("Missing semicolon in declaration"))
            | _ -> evalExpressionFloat tokens

        and evalExpressionFloat tokens =
            let (rem, v) = evalExpressionVal tokens
            (rem, valToFloat v)

        and evalExpressionVal tokens =
            (evalTermVal >> evalRestOfExpressionVal) tokens

        and evalRestOfExpressionVal (tokens, leftVal: Val) =
            match tokens with
            | Add :: tail ->
                let (tokenRemainder, termVal) = evalTermVal tail
                let result = valAdd leftVal termVal
                evalRestOfExpressionVal (tokenRemainder, result)
            | Sub :: tail ->
                let (tokenRemainder, termVal) = evalTermVal tail
                let result = valSub leftVal termVal
                evalRestOfExpressionVal (tokenRemainder, result)
            | _ -> (tokens, leftVal)

        and evalTermVal tokens = (evalUnaryVal >> evalRestOfTermVal) tokens

        and evalRestOfTermVal (tokens, leftVal: Val) =
            match tokens with
            | Mul :: tail ->
                let (tokenRemainder, factorVal) = evalUnaryVal tail
                let result = valMult leftVal factorVal
                evalRestOfTermVal (tokenRemainder, result)
            | Div :: tail ->
                let (tokenRemainder, factorVal) = evalUnaryVal tail
                // division-by-zero check
                match factorVal with
                | ValInt 0 -> raise (System.Exception("Division by zero"))
                | ValFloat 0.0 -> raise (System.Exception("Division by zero"))
                | _ ->
                    let result = valDiv leftVal factorVal
                    evalRestOfTermVal (tokenRemainder, result)
            | Rem :: tail ->
                let (tokenRemainder, factorVal) = evalUnaryVal tail
                let result = valRem leftVal factorVal
                evalRestOfTermVal (tokenRemainder, result)

            | (NumInt _ | NumFloat _ | Identifier _ | Lpar) :: _ ->
                let (tokenRemainder, factorVal) = evalPowerVal tokens
                let result = valMult leftVal factorVal
                evalRestOfTermVal (tokenRemainder, result)

            | _ -> (tokens, leftVal)


        and evalUnaryVal tokens =
            match tokens with
            | Sub :: tail -> 
                let (tokenRemainder, v) = evalUnaryVal tail
                let neg =
                    match v with
                    | ValInt i -> ValInt (-i)
                    | ValFloat f -> ValFloat (-f)
                    | _ -> raise (System.Exception("Unary - applied to non-numeric"))
                (tokenRemainder, neg)
            | _ -> evalPowerVal tokens

        and evalPowerVal tokens =
            let (remainderAfterBase, baseVal) = evalFactorVal tokens
            match remainderAfterBase with
            | Power :: tail ->
                let (tokenRemainder, exponentVal) = evalPowerVal tail
                match baseVal, exponentVal with
                | ValInt baseInt, ValInt exponentInt when exponentInt >= 0 ->
                    let powerResult = valPow baseVal exponentVal
                    (tokenRemainder, powerResult)
                | _ ->
                    // promote to float and use Math.Pow
                    let baseFloat = valToFloat baseVal
                    let exponentFloat = valToFloat exponentVal
                    (tokenRemainder, ValFloat (baseFloat ** exponentFloat))
            | _ -> (remainderAfterBase, baseVal)

        and evalFactorVal tokens =
            match tokens with 
            | NumInt n :: tail -> (tail, ValInt n)
            | NumFloat f :: tail -> (tail, ValFloat f)
            | Identifier name :: tail ->
                if name = "sin" || name = "cos" || name = "tan" then
                    let (rest, argVal) = evalFactorVal tail
                    let valAsFloat = valToFloat argVal
                    match name with
                    | "sin" -> (rest, ValFloat (Math.Sin valAsFloat))
                    | "cos" -> (rest, ValFloat (Math.Cos valAsFloat))
                    | "tan" -> (rest, ValFloat (Math.Tan valAsFloat))
                    | _ -> failwith "unreachable"
                else if name = "exp" || name = "log" then
                    let (rest, argVal) = evalFactorVal tail
                    let valAsFloat = valToFloat argVal
                    match name with
                    | "exp" -> (rest, ValFloat (Math.Exp valAsFloat))
                    | "log" -> (rest, ValFloat (Math.Log10 valAsFloat))
                    | _ -> failwith "unreachable"
                else
                    // variable lookup
                    if Map.containsKey name !symbolTable then
                        let variableFound = (!symbolTable).[name].value
                        match variableFound with
                        | None -> raise (System.Exception("Variable used before initialisation"))
                        | Some v -> (tail, v)
                    else
                        raise parseError
            | Lpar :: tail ->
                let (tokenRemainder, termVal) = evalExpressionVal tail
                match tokenRemainder with 
                | Rpar :: rest -> (rest, termVal)
                | _ -> raise parseError
            | _ -> raise parseError

        evalStatement tokenList

















namespace FSharpLib

module Interpreter =
    open System
    open FSharpLib.Numera

    // Convert ValType to an interpreter-friendly string
    let private valTypeToString (t: ValType) =
        match t with
        | IntType   -> "int"
        | FloatType -> "float"
        | BoolType  -> "bool"
        | StringType -> "string"

    // Convert Val to an interpreter-friendly string
    let private valToString (v: Val) =
        match v with
        | ValInt i    -> string i
        | ValFloat f  -> string f
        | ValBool b   -> string b
        | ValString s -> "\"" + s + "\""


    let GetVariables () : string =
        !symbolTable // dereferences the ref
        |> Map.toList
        |> List.map (fun (name, symbol) ->
            let valueStr =
                match symbol.value with
                | None      -> "<uninitialised>"
                | Some v    -> valToString v
            sprintf "%s : %s = %s" name (valTypeToString symbol.valType) valueStr
        )
        |> String.concat Environment.NewLine

    /// Evaluate one or more statements and return the result of the last one
    /// This is what the GUI calls     
    let Evaluate (source:string) : string =
        try
            let tokens = lexer source

            // Evaluate all statements in sequence, using the shared symbolTable
            let rec evalAll tokens lastValue =
                match tokens with
                | [] -> lastValue
                | _ ->
                    let (rest, value) = parseAndEval tokens

                    // If the parser didn't consume anything, avoid infinite loop
                    if rest = tokens then
                        value
                    else
                        evalAll rest value

            match tokens with
            | [] ->
                // Empty input -> no result
                ""
            | _ ->
                let finalVal = evalAll tokens 0.0
                finalVal.ToString()
        with ex ->
            "Error: " + ex.Message



    /// ------------------------- Plotting support for WPF ------------------------

    /// Parse a graph command like:
    ///   graph y = x^2, (-10, 10);
    ///   graphdif y = sin(x), (-5, 5);
    ///   graphint y = x^3, (-2, 2);
    ///   findroot y = x^2 - 4, (-10, 10);
    /// into (variable name, expression string, xmin, xmax)
    let private parseGraphCommand (input : string) =
        let trimmed =
            input.Trim()
                 .TrimEnd(';')

        let firstWord =
            trimmed.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.head
            |> fun s -> s.ToLowerInvariant()

        if not (firstWord = "graph" || firstWord = "graphdif" || firstWord = "graphint" || firstWord = "findroot") then
            failwith "Plot command must start with 'graph', 'graphdif', 'graphint', or 'findroot'. Example: graph x = x^2, (-10, 10);"

        // Drop the keyword
        let afterKeyword = trimmed.Substring(firstWord.Length).Trim()    // e.g. "y = x^2, (-10, 10)"

        // Split at the first comma: left is "y = x^2", right is "(-10, 10)"
        let commaIndex = afterKeyword.IndexOf(',')
        if commaIndex < 0 then
            // allow missing explicit range (defaults in GetPointsForExpression)
            let exprPart = afterKeyword.Trim()
            let equalsIndex = exprPart.IndexOf('=')
            if equalsIndex < 0 then failwith "Graph command must be of the form: graph x = expression, (min, max);"
            let varName    = exprPart.Substring(0, equalsIndex).Trim()
            let exprString = exprPart.Substring(equalsIndex + 1).Trim()
            // default range
            firstWord, varName, exprString, -10.0, 10.0
        else
            let exprPart  = afterKeyword.Substring(0, commaIndex).Trim()   // "y = x^2"
            let rangePart = afterKeyword.Substring(commaIndex + 1).Trim()  // "(-10, 10)"

            // Extract variable name and expression from "x = x^2"
            let equalsIndex = exprPart.IndexOf('=')
            if equalsIndex < 0 then failwith "Graph command must be of the form: graph y = expression, (min, max);"

            let varName    = exprPart.Substring(0, equalsIndex).Trim()       // "x"
            let exprString = exprPart.Substring(equalsIndex + 1).Trim()      // "x^2"

            if String.IsNullOrWhiteSpace(varName) then failwith "Graph command must specify a variable name, e.g. graph y = x^2, (-10, 10);"

            // Parse range "(min, max)"
            if not (rangePart.StartsWith("(") && rangePart.EndsWith(")")) then failwith "Range must be in the form (min, max), e.g. (-10, 10)."

            let rangeClean = rangePart.TrimStart('(').TrimEnd(')').Trim() // "-10, 10"

            let parts = rangeClean.Split([|','|], StringSplitOptions.RemoveEmptyEntries)

            if parts.Length <> 2 then failwith "Range must contain exactly two values: (min, max)."

            let parseDouble (s : string) = System.Double.Parse(s.Trim(), Globalization.CultureInfo.InvariantCulture)

            let xmin = parseDouble parts.[0]
            let xmax = parseDouble parts.[1]

            if xmin >= xmax then failwith "Range must satisfy min < max."

            (firstWord, varName, exprString, xmin, xmax)


    /// Evaluate an expression string with a particular variable bound to x.
    /// This uses existing lexer + parseAndEval + symbolTable.
    let private evalExpressionWithVar (varName : string) (exprString : string) (x : float) : float =
        // Save the old bindings for the two names we will touch (if any)
        let oldX = Map.tryFind "x" !symbolTable
        let oldVar = if varName = "x" then oldX else Map.tryFind varName !symbolTable

        try
            let symbol = { valType = FloatType; value = Some (ValFloat x) }
            // Add or override only the two entries we need
            symbolTable := Map.add "x" symbol !symbolTable
            if varName <> "x" then
                symbolTable := Map.add varName symbol !symbolTable

            // Tokenise and evaluate with temporary bindings
            let tokens = lexer exprString
            let (_, result) = parseAndEval tokens
            result
        finally
            // Restore bindings individually (remove if previously absent)
            match oldX with
            | None -> symbolTable := Map.remove "x" !symbolTable
            | Some s -> symbolTable := Map.add "x" s !symbolTable

            if varName <> "x" then
                match oldVar with
                | None -> symbolTable := Map.remove varName !symbolTable
                | Some s -> symbolTable := Map.add varName s !symbolTable



    /// Public function for the WPF plot window.
    /// Expects the full 'graph' command as typed in the GUI, e.g.:
    ///   graph x = x^2, (-10, 10);
    /// Returns a list of (x, y) points sampling the function over [xmin, xmax].
    let GetPointsForExpression (input : string) : (float * float) list =
        let mode, varName, exprString, xmin, xmax = parseGraphCommand input

        // Number of samples between xmin and xmax
        let steps = 200
        let step  = (xmax - xmin) / float steps

        let evalAt x =
            try evalExpressionWithVar varName exprString x
            with _ -> nan

        match mode with
        | "graph" ->
            [ for i in 0 .. steps ->
                let x = xmin + float i * step
                let y = evalAt x
                (x, y) ]

        | "graphdif" ->
            // central difference
            let h = (xmax - xmin) / float steps / 1000.0 |> max 1e-6
            [ for i in 0 .. steps ->
                let x = xmin + float i * step
                let yPlus = evalAt (x + h)
                let yMinus = evalAt (x - h)
                let deriv =
                    if System.Double.IsNaN(yPlus) || System.Double.IsNaN(yMinus)
                    then nan
                    else (yPlus - yMinus) / (2.0 * h)
                (x, deriv) ]

        | "graphint" ->
            // Uses the trapezoidal rule
            [ for sampleIndex in 0 .. steps ->
                let currentX = xmin + float sampleIndex * step

                // Number of subintervals used for numerical integration
                let subIntervals = 200
                let integrationStartX = xmin
                let integrationEndX = currentX
                let deltaX =
                    if subIntervals = 0 then 0.0
                    else (integrationEndX - integrationStartX) / float subIntervals

                // Trapezoidal integration
                let rec integrateSubIntervals (currentSubIntervalIndex : int) (accumulatedArea : float) =
                    if currentSubIntervalIndex > subIntervals then accumulatedArea
                    else
                        let evaluationX = integrationStartX + float currentSubIntervalIndex * deltaX
                        let functionValueAtX = evalAt evaluationX

                        let trapezoidWeight =
                            if currentSubIntervalIndex = 0 || currentSubIntervalIndex = subIntervals then 0.5
                            else 1.0

                        integrateSubIntervals (currentSubIntervalIndex + 1) (accumulatedArea + trapezoidWeight * functionValueAtX)

                // Final integral value for this x
                let integratedValue =
                    if System.Double.IsNaN integrationStartX || System.Double.IsNaN integrationEndX then nan
                    else deltaX * integrateSubIntervals 0 0.0

                (currentX, integratedValue) ]


        | "findroot" ->
            // Newton-Raphson with fallback to bisection if derivative is zero
            let initialGuess = (xmin + xmax) / 2.0
            let tolerance = 1e-10
            let maxIterations = 100
    
            let evalAt x =
                try evalExpressionWithVar varName exprString x
                with _ -> nan
    
            // 
            let derivativeAt x =
                let delta = 1e-6
                let yPlus = evalAt (x + delta)
                let yMinus = evalAt (x - delta)
                if System.Double.IsNaN(yPlus) || System.Double.IsNaN(yMinus) then nan
                else (yPlus - yMinus) / (2.0 * delta)
    
            // Newton-Raphson iteration
            let rec newtonIteration x count =
                if count >= maxIterations then None
                else
                    let currentVal = evalAt x
                    let derivVal = derivativeAt x
            
                    if System.Double.IsNaN(currentVal) || System.Double.IsNaN(derivVal) then None
                    elif abs derivVal < 1e-12 then None  // Derivative too small
                    else
                        let xNext = x - currentVal / derivVal
                        if abs (xNext - x) < tolerance then Some xNext
                        else newtonIteration xNext (count + 1)
    
            let root =
                match newtonIteration initialGuess 0 with
                | Some r -> r
                | None -> failwith "Newton-Raphson failed"
    
            [ (root, 0.0) ]

        | _ -> failwith "Unknown graph mode"
