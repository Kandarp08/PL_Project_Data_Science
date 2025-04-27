open Datatypes
open Dataframe
open Data_object
open Row

open Int_util
open Float_util
open Operations
open Int_transformations
open Float_transformations

module type LIB =
sig
    (* Apply a given function to a column in the dataframe *)
    val map : Dataframe.t -> (data_object -> data_object) -> string -> Dataframe.t

    (* Filter rows of a dataframe whose columns satisfy a given function *)
    val filter : Dataframe.t -> (data_object -> bool) -> string -> Dataframe.t

    (* Find a given element in a particular column of the dataframe *)
    val mem : Dataframe.t -> string -> data_object -> bool

    (* Update value of accumulator value by repeatedly applying a function *)    
    val fold_left : Dataframe.t -> string -> (data_object -> data_object -> data_object) -> data_object -> data_object
    
    (* Update value of accumulator value by repeatedly applying a function *)
    val fold_right : Dataframe.t -> string -> (data_object -> data_object -> data_object) -> data_object -> data_object
    
    (* Standardization of a given column of the dataframe *)
    val normalize : Dataframe.t -> string -> Dataframe.t

    (* Min-Max normalization of a given column of the dataframe *)
    val min_max_normalize : Dataframe.t -> string -> Dataframe.t

    (* Impute NULL values of integer/float column with the mean *)
    val imputena : Dataframe.t -> string -> Dataframe.t

    (* Replace NULL values of a given column with the given value *)
    val fillna : Dataframe.t -> string -> data_object -> Dataframe.t
    
    (**)
    val join : Dataframe.t -> Dataframe.t -> string -> Dataframe.t
    
    (* Sum of a given integer/float column of the dataframe *)
    val sum : Dataframe.t -> string -> data_object

    (* Length of a given column of the dataframe *)
    val len : Dataframe.t -> string -> int

    (* Mean of a given integer/float column of the dataframe *)
    val mean : Dataframe.t -> string -> float

    (* Standard deviation of a given integer/float column of the dataframe *)
    val stddev : Dataframe.t -> string -> float
end

module Lib : LIB