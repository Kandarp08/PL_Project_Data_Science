open Data_object
open Datatypes
open Utils

(** Operations on rows (lists of data objects) *)
module Row : sig
  type t = data_object list

  (** Display a row to standard output *)
  val display_row : t -> unit

  (** Create a row from a list *)
  val row_from_list : (datatype -> 'b -> Data_object.data_object) -> datatype list -> int -> 'b list -> t

  (** Create a row from a list of strings, applying the appropriate datatypes *)
  val row_from_string_list : datatype list -> int -> string list -> t

  (** Create a row from a list of strings, applying the appropriate datatypes *)
  val row_from_json_value_list : datatype list -> int -> json_value list -> t

  (* Converts a row into a CSV string *)
  val row_to_csv : datatype list -> data_object list -> string

  (* Converts a row into a JSON array *)
  val row_to_json_array : data_object list -> string
end