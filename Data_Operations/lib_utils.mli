open Datatypes
open Dataframe
open Data_object
open Row

module type LIB_UTILS = 
sig
    (* Given a Dataframe.t, replaces the elements of a given column with new values *)
    val convert : int -> data_object Seq.t -> data_object Seq.t -> Row.t Seq.t -> Row.t Seq.node
    
    (* Given a Dataframe.t, filters the elements of a given column based on a predicate function *)
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
    val string_of_data_object : data_object -> string
    
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
    val get_rows_as_list : Dataframe.t -> Row.t list
    
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

    val string_of_data_object : data_object -> string
end

module Lib_utils : LIB_UTILS