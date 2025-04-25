open Datatypes
open Dataframe
open Data_object
open Row

module type LIB_UTILS = 
sig
    val convert : int -> (data_object Seq.t) -> (data_object Seq.t) -> (Row.t Seq.t) -> (Row.t Seq.node)
    val filter_rows : int -> (data_object Seq.t) -> (data_object Seq.t) -> (Row.t Seq.t) -> (Row.t Seq.node)
end

module Lib_utils : LIB_UTILS