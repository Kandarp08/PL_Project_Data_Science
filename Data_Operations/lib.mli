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
    val map : Dataframe.t -> (data_object -> data_object) -> string -> Dataframe.t
    val filter : Dataframe.t -> (data_object -> bool) -> string -> Dataframe.t
    val mem : Dataframe.t -> string -> data_object -> bool
    val fold_left : Dataframe.t -> string -> (data_object -> data_object -> data_object) -> data_object -> data_object
    val fold_right : Dataframe.t -> string -> (data_object -> data_object -> data_object) -> data_object -> data_object
    val normalize : Dataframe.t -> string -> Dataframe.t
    val fillna : Dataframe.t -> string -> Dataframe.t
    val join : Dataframe.t -> Dataframe.t -> string -> Dataframe.t
end

module Lib : LIB