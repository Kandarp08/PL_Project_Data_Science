(** Types for representing data and datatypes *)

(** The supported datatypes *)
type datatype = 
  INT
| FLOAT
| STRING
| BOOL
| CHAR

(** Helper functions to work with datatype *)
module Datatype : sig
  type t = datatype
  val string_to_datatype : string -> datatype
end