(** Remove quotes and whitespace from a string *)
val strip : string -> string

(* Representing JSON data *)
type json_object = (string * value) list
and 
value =
    OBJECT of json_object
  | ARRAY of value list
  | STRING_VAL of string
  | NUMBER of string
  | BOOLEAN_VAL of bool
  | NULL_VAL

(* Functions to parse JSON strings *)
module JSON :
  sig
    type t = json_object
    val skip_whitespace : string -> int -> int
    val eat_comma : string -> int -> int
    val eat_colon : string -> int -> int
    val parse_string : string -> int -> int * string
    val parse_number : string -> int -> int * string
    val parse_keyword : string -> string -> int -> int
    val parse_value : string -> int -> int * value
    val parse_array : string -> int -> int * value list
    val parse_object : string -> int -> int * t
    val parse : string -> t
  end