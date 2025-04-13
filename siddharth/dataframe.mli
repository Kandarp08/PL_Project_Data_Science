open Data_object
open Datatypes
open Row

(** Dataframe type and operations *)

type dataframe = {
  headers: string list;
  dtypes: datatype list;
  rows: Row.t Seq.t;
  ncols: int;
}

module Dataframe : sig
  type t = dataframe

  (** Get the index of a column by name *)
  val get_column_index : dataframe -> string -> int

  (** Get a column by name as a sequence of data objects *)
  val get_column : dataframe -> string -> data_object Seq.t

  (** Load a dataframe from a CSV file *)
  val load_from_csv : string -> dataframe
end