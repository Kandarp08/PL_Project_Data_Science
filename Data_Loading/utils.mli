(** Remove quotes and whitespace from a string *)
val strip : string -> string

(* Add quotes to string *)
val get_output_string : string -> string

(* Representing JSON data *)
type json_object = (string * json_value) list
and 
json_value =
    OBJECT of json_object
  | ARRAY of json_value list
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
    val parse_value : string -> int -> int * json_value
    val parse_array : string -> int -> int * json_value list
    val parse_object : string -> int -> int * t
    val parse : string -> t
    val get_value : (string * json_value) list -> string -> json_value
  end

module JSON_Value:
  sig
    val string_value_to_string : json_value -> string
    val array_to_list : json_value -> json_value list
  end