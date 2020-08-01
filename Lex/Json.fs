module Lex.Json

open System
open Lex
open Parsing

type JValue =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list
    
let jNull =
    parseStr "null"
    >>% JNull
    <?> "null"

let jBool =
    let jTrue =
        parseStr "true"
        >>% JBool true
    let jFalse =
        parseStr "false"
        >>% JBool false 
    jTrue <|> jFalse
    <?> "bool"
    
let jUnescapedChar =
    let label = "char"
    satisfy (fun ch -> ch <> '\\' && ch <> '\"') label
    
let jEscapedChar =
    [
        ("\\\"", '\"')
        ("\\\\", '\\')
        ("\\/", '/')
        ("\\b", '\b')
        ("\\f", '\f')
        ("\\n", '\n')
        ("\\r", '\r')
        ("\\t", '\t')
    ]
    |> List.map (fun (toMatch,result) ->
        parseStr toMatch >>% result)
    |> choice
    <?> "escaped char"
    
let jUnicodeChar =
    let backslash = parseChar '\\'
    let uChar = parseChar 'u'
    let hexdigit = anyOf (['0'..'9']@['A'..'F']@['a'..'f'])
    let convertToChar (((h1,h2),h3),h4) =
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse(str,Globalization.NumberStyles.HexNumber) |> char
    backslash >>. uChar >>. hexdigit .>>. hexdigit .>>. hexdigit .>>.hexdigit
    |>> convertToChar
        
let quotedString =
    let quote = parseChar '\"' <?> "quote"
    let jchar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar
    quote >>. manyChars jchar .>> quote
    
let jString =
    quotedString
    |>> JString
    <?> "quoted string"
    
let jNumber =               
    let optSign = opt (parseChar '-')
    
    let zero = parseStr "0"

    let digitOneNine =
        satisfy (fun ch -> Char.IsDigit ch && ch <> '0') "1-9"
        
    let digit =
        satisfy (fun ch -> Char.IsDigit ch) "digit"
        
    let point =
        parseChar '.'
        
    let e = parseChar 'e' <|> parseChar 'E'

    let optPlusMinus = opt (parseChar '-' <|> parseChar '+')

    let nonZeroInt =
        digitOneNine .>>. manyChars digit
        |>> fun (first,rest) -> string first + rest

    let intPart = zero <|> nonZeroInt

    let fractionPart = point >>. manyChars1 digit

    let exponentPart = e >>. optPlusMinus .>>. manyChars1 digit

    let convertToJNumber (((optSign, intPart), fractionPart), expPart) =
        let signStr =
            optSign
            |>? string
            
        let fractionPartStr =
            fractionPart
            |>? (fun digits -> "." + digits)
            
        let expPartStr =
            expPart
            |>? fun (optSign, digits) ->
                let sign = optSign |>? string
                "e" + sign + digits
        
        (signStr + intPart + fractionPartStr + expPartStr)
        |> float
        |> JNumber

    optSign .>>. intPart .>>. opt fractionPart .>>. opt exponentPart
    |>> convertToJNumber
    <?> "number"

let jNumber_ = jNumber .>> spaces1

let jValue,jValueRef = createParserForwardedToRef<JValue>()

let jArray =
    let left = parseChar '[' .>> spaces
    let right = parseChar ']' .>> spaces
    let comma = parseChar ',' .>> spaces
    let value = jValue .>> spaces
    let values = sepBy1 value comma
    
    between left values right
    |>> JArray
    <?> "array"

let jObject =
    let left = parseChar '{' .>> spaces
    let right = parseChar '}' .>> spaces
    let colon = parseChar ':' .>> spaces
    let comma = parseChar ',' .>> spaces
    let key = quotedString .>> spaces
    let value = jValue .>> spaces
    
    let keyValue = (key .>> colon) .>>. value
    let keyValues = sepBy1 keyValue comma
    
    between left keyValues right
    |>> Map.ofList
    |>> JObject
    <?> "object"

jValueRef := choice
    [
    jNull
    jBool
    jNumber
    jString
    jArray
    jObject
    ]