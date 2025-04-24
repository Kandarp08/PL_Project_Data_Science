open Datatypes
open Utils


(* Reprsentation of a data element*)
type data_object = 
  STRING_DATA of string 
| FLOAT_DATA of float
| BOOL_DATA of bool
| CHAR_DATA of char
| INT_DATA of int
| NULL

(** Operations on data objects *)
module DataObject : sig
  type t = data_object

  (** Convert a string to a boolean data object *)
  val bool_data_from_string : string -> data_object

  (** Convert a data object to its string representation *)
  val to_string : data_object -> string

  (** Convert a string to a data object of the specified type *)
  val from_string : datatype -> string -> data_object

  (** Convert a json_value to a data object of the specified type *)
  val from_json_value : datatype -> json_value -> data_object
end