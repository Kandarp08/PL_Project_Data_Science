(** Types for representing data and datatypes *)

(** The supported datatypes *)
type datatype = 
  INT
| FLOAT
| STRING
| BOOL
| CHAR

(** Convert a string representation to a datatype *)
val string_to_datatype : string -> datatype