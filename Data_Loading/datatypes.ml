open Utils

type datatype = 
  INT
| FLOAT
| STRING
| BOOL
| CHAR

module Datatype = struct
  type t = datatype
  let string_to_datatype str = 
    match String.lowercase_ascii (strip str) with
      "str"
    | "string" -> STRING
    | "int" 
    | "integer" -> INT
    | "bool"
    | "boolean" -> BOOL
    | "char"
    | "character" -> CHAR
    | "float" -> FLOAT
    | _ -> failwith "Invalid datatype"

  let datatype_to_string dtype = 
    match dtype with
    | STRING -> "String"
    | INT -> "Int"
    | FLOAT -> "Float"
    | BOOL -> "Bool"
    | CHAR -> "Char"

end