open Datatypes
open Dataframe
open Data_object
open Row

open Int_util
open Float_util
open Operations
open Int_transformations
open Float_transformations
open Lib_utils

module type LIB =
sig
    (* Diplay the contents of the entire dataframe *)
    val show_df : Dataframe.t -> unit

    (* Apply a given function to a column in the dataframe *)
    val map : (data_object -> data_object) -> string -> Dataframe.t -> Dataframe.t

    (* Filter rows of a dataframe whose columns satisfy a given function *)
    val filter : (data_object -> bool) -> string -> Dataframe.t -> Dataframe.t

    (* Find a given element in a particular column of the dataframe *)
    val mem : string -> data_object -> Dataframe.t -> bool

    (* Update value of accumulator by repeatedly applying a function *)    
    val fold_left : string -> (data_object -> data_object -> data_object) -> data_object -> Dataframe.t -> data_object
    
    (* Update value of accumulator by repeatedly applying a function *)
    val fold_right : string -> (data_object -> data_object -> data_object) -> data_object -> Dataframe.t -> data_object
    
    (* Standardization of a given column of the dataframe *)
    val normalize : string -> Dataframe.t -> Dataframe.t

    (* Min-Max normalization of a given column of the dataframe *)
    val min_max_normalize : string -> Dataframe.t -> Dataframe.t

    (* Impute NULL values of integer/float column with the mean *)
    val imputena : string -> Dataframe.t -> Dataframe.t

    (* Replace NULL values of a given column with the given value *)
    val fillna : string -> data_object -> Dataframe.t -> Dataframe.t
    
    (**)
    val join : Dataframe.t -> Dataframe.t -> string -> Dataframe.t
    
    (* Sum of a given integer/float column of the dataframe *)
    val sum : string -> Dataframe.t -> data_object

    (* Length of a given column of the dataframe *)
    val len : string -> Dataframe.t -> data_object

    (* Mean of a given integer/float column of the dataframe *)
    val mean : string -> Dataframe.t -> data_object

    (* Standard deviation of a given integer/float column of the dataframe *)
    val stddev : string -> Dataframe.t -> data_object

    (* Add a new row to the dataframe *)
    val add_row : Row.t -> Dataframe.t -> Dataframe.t
    
    (* Delete the rows which satisfy a given condition *)
    val delete_row : (Row.t -> bool) -> Dataframe.t -> Dataframe.t

    (* Update the rows of the dataframe that satisfy a given condition *)
    val update_row : (Row.t -> bool) -> (Row.t -> Row.t) -> Dataframe.t -> Dataframe.t

    (* Group the values of a given column, while applying given functions on the column values *)
    val groupByAggregate : string -> (string * (string -> Dataframe.t -> data_object)) list -> Dataframe.t -> Dataframe.t

    (* Get rows of a dataframe by providing range of row indices *)
    val iloc : int -> int -> Dataframe.t -> Dataframe.t

    (* Get rows of a dataframe by providing range of row labels *)
    val loc : string -> string -> string -> Dataframe.t -> Dataframe.t
end

module Lib : LIB