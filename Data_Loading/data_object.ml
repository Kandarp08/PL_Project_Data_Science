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

  let from_json_value dtype jval  = 
    try
      match jval with 
        STRING_VAL (str) -> 
          if dtype = STRING then STRING_DATA (str)
          else if dtype = CHAR then CHAR_DATA (str.[0])
          else failwith "Invalid value for the given datatype"
      | NUMBER (n) -> 
        if dtype = FLOAT then FLOAT_DATA (float_of_string n)
        else if dtype = INT then INT_DATA (int_of_string n)
        else failwith "Invalid value for the given datatype"
      | BOOLEAN_VAL (b) -> if dtype = BOOL then BOOL_DATA (b) else failwith "Invalid value for the given datatype"
      | _ -> NULL
    with
      _ -> NULL

  let get_datatype data = 
    match data with 
    | STRING_DATA (_) -> STRING
    | FLOAT_DATA (_) -> FLOAT
    | BOOL_DATA (_) -> BOOL
    | CHAR_DATA (_) -> CHAR
    | INT_DATA (_) -> INT
    | _ -> failwith "Value cannot be NULL"
end