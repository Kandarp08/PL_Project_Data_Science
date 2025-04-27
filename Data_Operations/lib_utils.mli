open Datatypes
open Dataframe
open Data_object
open Row

module type LIB_UTILS = 
sig
    (* Given a dataframe, replaces the elements of a given column with new values *)
    val convert : int -> data_object Seq.t -> data_object Seq.t -> Row.t Seq.t -> Row.t Seq.node
    
    (* Given a dataframe, filters the elements of a given column based on a predicate function *)
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
    val seq_to_list : 'a Seq.t -> 'a list
    
    (**)
    val get_rows_as_list : Dataframe.t -> Row.t list
    
    (**)
    val joinItemWithList : data_object list -> data_object list list -> string -> Dataframe.t -> Dataframe.t -> data_object list list
    
    (**)
    val convertToDataFrame : Row.t list -> Dataframe.t -> Dataframe.t -> Dataframe.t
end

module Lib_utils : LIB_UTILS