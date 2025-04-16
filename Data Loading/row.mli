open Data_object
open Datatypes

(** Operations on rows (lists of data objects) *)
module Row : sig
  type t = data_object list

  (** Display a row to standard output *)
  val display_row : t -> unit

  (** Create a row from a list of strings, applying the appropriate datatypes *)
  val row_from_list : string list -> int -> datatype list -> t
end