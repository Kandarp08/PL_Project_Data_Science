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
    
    (* Removes an element at the specified index from a list *)
    val removeElement_atIndex : int -> 'a list -> 'a list
    
    (* Removes a field (column value) from a row based on column name *)
    val removeField : data_object list -> string -> Dataframe.t -> data_object list
    
    (* Helper function to find a value at a target index in a row *)
    val findValueHelper : int -> 'a list -> int -> 'a
    
    (* Finds a value in a row based on column name in a dataframe *)
    val findValue : string -> 'a list -> Dataframe.t -> 'a
    
    (* Merges two records into a single record when they have matching values in the specified column *)
    val mergeIntoSingleRecord : data_object list -> data_object list -> string -> Dataframe.t -> Dataframe.t -> data_object list

    (* Converts the row sequence of a dataframe to a list for easier processing *)
    val get_rows_as_list : Dataframe.t -> Row.t list
    
    (* Joins an item with each element in a list based on a common column *)
    val joinItemWithList : data_object list -> data_object list Seq.t -> string -> Dataframe.t -> Dataframe.t -> data_object list Seq.t
    
    (* Converts a sequence of rows into a dataframe with combined headers from two parent dataframes *)
    val convertToDataFrame : Row.t Seq.t -> Dataframe.t -> Dataframe.t -> Dataframe.t

    (* Creates a new dataframe with the same schema as the parent but with different rows *)
    val convertRowsToDataframe : Dataframe.t -> Row.t Seq.t -> Dataframe.t

    (* Creates a dataframe from a dataframeLoc and a sequence of rows *)
    val convertRowsToDataframeLoc : dataframeLoc -> Row.t Seq.t -> Dataframe.t
    
    (* Extracts an integer value from a data_object *)
    val extract_int : data_object -> int
    
    (* Extracts a float value from a data_object (converts INT to float if needed) *)
    val extract_float : data_object -> float
    
    (* Converts a sequence of data_objects to a sequence of integers *)
    val get_int_values : data_object Seq.t -> int Seq.t
    
    (* Converts a sequence of data_objects to a sequence of floats *)
    val get_float_values : data_object Seq.t -> float Seq.t
    
    (* Converts a sequence to a list while preserving order *)
    val seq_to_list : 'a Seq.t -> 'a list
    
    (* Applies an aggregation function to a specific column in a dataframe *)
    val singleAggregateResult : Dataframe.t -> string -> (string -> Dataframe.t -> data_object) -> data_object
    
    (* Checks if an element is a member of a sequence *)
    val isMemOfSeq : 'a -> 'a Seq.t -> bool
    
    (* Returns a sequence of unique values in a specific column of a dataframe *)
    val getUniqueValues : Dataframe.t -> string -> data_object Seq.t
    
    (* Applies a single column aggregation function based on a column name and function mapping *)
    val applyOneColumnAggregate : string * (string -> Dataframe.t -> data_object) -> Dataframe.t -> data_object
    
    (* Creates a row by filtering the dataframe for rows matching a value and applying aggregation functions *)
    val createRow : Dataframe.t -> string -> (string * (string -> Dataframe.t -> 'a)) list -> data_object -> 'a list

    (* Computes the width needed for each column when displaying a dataframe *)
    val compute_column_widths : Dataframe.t -> int list

    (* Prints a horizontal separator line with the given column widths *)
    val print_separator : int list -> unit

    (* Converts a data_object to a string representation *)
    val string_of_data_object : data_object -> string

    (* Helper function for iloc to extract a slice of rows from a sequence *)
    val iloc_helper : 'a Seq.t -> int -> int -> int -> 'a Seq.t -> bool -> (bool * 'a Seq.t)

    (* Creates a dataframeLoc with row indices based on values in a specified column *)
    val set_index : string -> Dataframe.t -> dataframeLoc
end

module Lib_utils : LIB_UTILS
