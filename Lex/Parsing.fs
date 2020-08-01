module Lex.Parsing

// ==================================
// General
// ==================================

open System


type Position =
    { line: int
      column: int }

type InputState =
    { lines: string []
      position: Position }

let initalPos =
    { line = 0
      column = 0 }

let incrementColumn pos =
    { pos with column = pos.column + 1 }

let incrementLine pos =
    { line = pos.line + 1
      column = 0 }


type ParserPosition = {
    currentLine: string
    line: int
    column: int
}

let fromString str =
    if String.IsNullOrEmpty(str) then
        { lines = [||]
          position = initalPos }
    else
        let lines = str.Split(Environment.NewLine, StringSplitOptions.None)
        { lines = lines
          position = initalPos }

let currentLine inputState =
    let linePos = inputState.position.line
    if linePos < inputState.lines.Length then
        inputState.lines.[linePos]
    else
        "end of file"

let nextChar input =
    let linePos = input.position.line
    let colPos = input.position.column
    
    if linePos >= input.lines.Length then
        input, None
    else
        let currentLine = currentLine input
        if colPos < currentLine.Length then
            let char = currentLine.[colPos]
            let newPos = incrementColumn input.position
            let newState = {input with position = newPos}
            newState, Some char
        else
            let char = '\n'
            let newPos = incrementLine input.position
            let newState = {input with position = newPos}
            newState, Some char

// ==================================
// Parsing
// ==================================

type ParserLabel = string

type ParserError = string

type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition
            
type Parser<'a> =
    { parserFn: InputState -> Result<'a * InputState>
      label: ParserLabel }

let runOnInput (parser:Parser<'a>) input =
    parser.parserFn input

/// Run a parser with some input.
let run parser input =
    runOnInput parser (fromString input)

// ==================================
// Errors
// ==================================

let parserPositionFromInputState inputState = {
    currentLine = currentLine inputState
    line = inputState.position.line
    column = inputState.position.column
}

let printResult result =
    match result with
    | Success (value, input) -> printfn "%A" value
    | Failure (label, error, parserPos) ->
        let errorLine = parserPos.currentLine
        let colPos = parserPos.column
        let linePos = parserPos.line
        let failureCaret = sprintf "%*s^%s" colPos "" error
        printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret

// ==================================
// Labels
// ==================================

let getLabel parser =
    parser.label

let setLabel parser newLabel =
    let newInnerFn input =
        let result = parser.parserFn input
        match result with
        | Success s -> Success s
        | Failure (oldLabel, err, pos) -> Failure(newLabel, err, pos)
    { parserFn = newInnerFn
      label = newLabel }

let (<?>) = setLabel

// ==================================
// Combinators
// ==================================

let satisfy predicate label =
    let innerFn input =
        let remainingInput, charOpt = nextChar input
        match charOpt with
        | None ->
            let err = "No more input"
            let pos = parserPositionFromInputState input
            Failure (label,err,pos)
        | Some first ->
            if predicate first then
                Success (first,remainingInput)
            else
                let err = sprintf "Unexpected '%c'" first
                let pos = parserPositionFromInputState input
                Failure (label,err,pos)
    { parserFn = innerFn
      label = label }

/// Bind a parser production function (f) with a parser (p).
/// The output of p is passed into f to create a new parser.
let bindParsers f p =
    let label = "unknown"

    let innerFn input =
        let result1 = runOnInput p input
        match result1 with
        | Failure (label, err, pos) -> Failure(label, err, pos)
        | Success (value1, remainingInput) ->
            let p2 = f value1
            runOnInput p2 remainingInput
    { parserFn = innerFn
      label = label }

let (>>=) p f = bindParsers f p

/// Lift a value to a parser
let returnParser x =
    let label = sprintf "%A" x
    let innerFn input =
        Success(x, input)
    { parserFn = innerFn
      label = label }

/// Apply a function to a value in a parser.
let mapParser f =
    bindParsers (f >> returnParser)

/// Infix version of mapParser
let (<!>) = mapParser

/// "Piping" version of mapParser
let (|>>) x f = mapParser f x

/// Apply a wrapped function to a wrapped value.
let applyParser fP xP =
    fP >>= (fun f -> xP >>= (fun x -> returnParser (f x)))

let (<*>) = applyParser

/// Lift a two parameter function to 'parser world'.
let lift2 f xP yP =
    returnParser f <*> xP <*> yP

let andThen parser1 parser2 =
    let label = sprintf "%s and then %s" (getLabel parser1) (getLabel parser2)
    parser1
    >>= (fun p1Result -> parser2 >>= (fun p2Result -> returnParser (p1Result, p2Result)))
    <?> label

let (.>>.) = andThen

/// Combine 2 parsers as "a or else b"
let orElse parser1 parser2 =
    let label = sprintf "%s or else %s" (getLabel parser1) (getLabel parser2)

    let innerFn input =
        let result1 = runOnInput parser1 input
        match result1 with
        | Success result ->
            result1
        | Failure _ ->
            let result2 = runOnInput parser2 input
            result2
    { parserFn = innerFn
      label = label }

let (<|>) = orElse

let choice listOfParsers =
    List.reduce (<|>) listOfParsers

let rec sequence parserList =
    let cons head tail = head :: tail
    let consP = lift2 cons
    match parserList with
    | [] -> returnParser []
    | head :: tail -> consP head (sequence tail)
    
let rec parseZeroOrMore parser input =
    let firstResult = runOnInput parser input
    match firstResult with
    | Failure (_,_,_) -> ([], input)
    | Success (firstResult, inputAfterFirstParse) ->
        let (subsequentValues, remainingInput) =
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstResult :: subsequentValues
        (values, remainingInput)

let many parser =
    let label = sprintf "many %s" (getLabel parser)
    let rec innerFn input =
        Success(parseZeroOrMore parser input)
    { parserFn = innerFn
      label = label }

/// Match one or more occurrence's of the specific parser
let many1 parser =
    parser >>= (fun head -> many parser >>= (fun tail -> returnParser (head :: tail)))

let opt p =
    let some = p |>> Some
    let none = returnParser None
    some <|> none

let (.>>) p1 p2 =
    p1 .>>. p2 |> mapParser (fun (a, b) -> a)

let (>>.) p1 p2 =
    p1 .>>. p2 |> mapParser (fun (a, b) -> b)

let between p1 p2 p3 =
    p1 >>. p2 .>> p3

/// Parses one or more occurrences of p separated by sep.
let sepBy1 p sep =
    let sepThenP = sep >>. p
    p .>>. many sepThenP |>> fun (p, pList) -> p :: pList

/// Parses zero or more occurrences of p separated by sep.
let sepBy p sep =
    sepBy1 p sep <|> returnParser []

// ==================================
// Parsers
// ==================================

// ----------------------------------
// Strings
// ----------------------------------

let parseChar charToMatch =
    let predicate ch = (ch = charToMatch)
    let label = sprintf "%c" charToMatch
    satisfy predicate label

let anyOf listOfChars =
    let label = sprintf "any of %A" listOfChars
    listOfChars
    |> List.map parseChar
    |> choice
    <?> label
    
    
let charListToStr charList =
    String(List.toArray charList)
    
let manyChars charParser =
    many charParser
    |>> charListToStr
    
let manyChars1 charParser =
    many charParser
    |>> charListToStr
    
let parseStr str =
    let label = str
    str
    |> List.ofSeq
    |> List.map parseChar
    |> sequence
    |> mapParser charListToStr
    <?> label
    
// ----------------------------------
// Whitespace
// ----------------------------------

let whitespaceChar =
    let predicate = Char.IsWhiteSpace
    let label = "whitespace"
    satisfy predicate label
    
let spaces = many whitespaceChar

let spaces1 = many1 whitespaceChar

// ----------------------------------
// Numbers
// ----------------------------------

let digitChar =
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label
    
let parseInt =
    let label = "integer"
    let resultToInt (sign,digits) =
        let i = digits |> int
        match sign with
        | Some ch -> -i
        | None -> i
        
    let digits = manyChars1 digitChar
    
    opt (parseChar '-') .>>. digits
    |> mapParser resultToInt
    <?> label
    
let parseFloat =
    let label = "float"
    let resultToFloat(((sign,digits1), point), digits2) =
        let fl = sprintf "%s.%s" digits1 digits2 |> float
        match sign with
        | Some ch -> -fl
        | None -> fl
        
    let digits = manyChars1 digitChar
    
    opt (parseChar '-') .>>. digits .>>. parseChar '.' .>>. digits
    |> mapParser resultToFloat
    <?> label