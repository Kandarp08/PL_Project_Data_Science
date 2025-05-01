open Datatypes
open Dataframe
open Data_object
open Row

open Operations
open Int_util
open Float_util

type dataframeLoc = {
    lheaders: string list;
    ldtypes: datatype list;
    lrows: Row.t Seq.t;
    lncols: int;
    lindices: (string, int) Hashtbl.t option; (* Maps row labels to row indices *)
}

module type LIB_UTILS = 
sig
    (* Updates the value at a given postion in the list *)
    val update_pos : 'a list -> 'a -> int -> int -> 'a list

    (* Given a dataframe, returns a new dataframe in which values of a given column are replaced by new ones *)
    val convert : int -> data_object Seq.t -> data_object Seq.t -> Row.t Seq.t -> Row.t Seq.node
    
    (* Given a dataframe, returns a new dataframe which contains only the filtered rows *)
    val filter_rows : int -> data_object Seq.t -> data_object Seq.t -> Row.t Seq.t -> Row.t Seq.node
    
    (**)
    val removeElement_atIndex : int -> 'a list -> 'a list
    
    (**)
    val removeField : data_object list -> string -> Dataframe.t -> data_object list
    
    (**)
    val findValueHelper : int -> 'a list -> int -> 'a
    
    (**)
    val findValue : string -> 'a list -> Dataframe.t -> 'a
    
    (**)
    val mergeIntoSingleRecord : data_object list -> data_object list -> string -> Dataframe.t -> Dataframe.t -> data_object list
    
    (**)
    val get_rows_as_list : Dataframe.t -> Row.t list
    
    (**)
    val joinItemWithList : data_object list -> data_object list list -> string -> Dataframe.t -> Dataframe.t -> data_object list list
    
    (**)
    val convertToDataFrame : Row.t list -> Dataframe.t -> Dataframe.t -> Dataframe.t

    (**)
    val convertRowsToDataframe : Dataframe.t -> Row.t Seq.t -> Dataframe.t

    (**)
    val convertRowsToDataframeLoc : dataframeLoc -> Row.t Seq.t -> Dataframe.t
    
    (**)
    val extract_int : data_object -> int
    
    (**)
    val extract_float : data_object -> float
    
    (**)
    val get_int_values : data_object Seq.t -> int Seq.t
    
    (**)
    val get_float_values : data_object Seq.t -> float Seq.t
    
    (**)
    val seq_to_list : 'a Seq.t -> 'a list
    
    (**)
    val singleAggregateResult : Dataframe.t -> string -> (string -> Dataframe.t -> data_object) -> data_object
    
    (**)
    val isMemOfSeq : 'a -> 'a Seq.t -> bool
    
    (**)
    val getUniqueValues : Dataframe.t -> string -> data_object Seq.t
    
    (**)
    val applyOneColumnAggregate : string * (string -> Dataframe.t -> data_object) -> Dataframe.t -> data_object
    
    (**)
    val createRow : Dataframe.t -> string -> (string * (string -> Dataframe.t -> 'a)) list -> data_object -> 'a list

    (**)
    val compute_column_widths : Dataframe.t -> int list

    (**)
    val print_separator : int list -> unit

    (**)
    val string_of_data_object : data_object -> string

    (**)
    val iloc_helper : 'a Seq.t -> int -> int -> int -> 'a Seq.t -> bool -> (bool * 'a Seq.t)

    (**)
    val set_index : string -> Dataframe.t -> dataframeLoc
end

module Lib_utils : LIB_UTILS