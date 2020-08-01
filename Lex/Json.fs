module Lex.Json

open Lex
open Parsing


type JValue =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list
    
