open Utils
open Datatypes

type data_object = 
  STRING_DATA of string 
| FLOAT_DATA of float
| BOOL_DATA of bool
| CHAR_DATA of char
| INT_DATA of int
| NULL


(* Some functions for the data_object variant *)
module DataObject = struct 
  type t = data_object
  let bool_data_from_string data = 
    match String.lowercase_ascii data with
      "1"
    | "t"
    | "true" -> BOOL_DATA (true)
    | "0"
    | "f"
    | "false" -> BOOL_DATA (false)
    | _ -> NULL

  let to_string data = 
    match data with
        STRING_DATA (d) -> d
      | INT_DATA (d) -> string_of_int d
      | CHAR_DATA (d) -> String.make 1 d 
      | BOOL_DATA (d) -> string_of_bool d
      | FLOAT_DATA (d) -> string_of_float d
      | NULL -> "NULL"

  let from_string dtype data  = 
    try
      let sdata = strip data in
      match dtype with
        STRING -> if data = "" then NULL else STRING_DATA (sdata)
      | INT -> INT_DATA (int_of_string sdata)
      | FLOAT -> FLOAT_DATA (float_of_string sdata)
      | CHAR -> CHAR_DATA (sdata.[0]) 
      | BOOL -> bool_data_from_string sdata
    with
      _ -> NULL
end