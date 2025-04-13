open Utils

type datatype = 
  INT
| FLOAT
| STRING
| BOOL
| CHAR

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