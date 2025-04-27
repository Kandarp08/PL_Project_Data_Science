open Data_object
open Data_object.DataObject

open Int_util
open Operations

(* Common transformations that can be applied on a column *)
module type INT_TRANSFORMATIONS = 
sig
    (* Apply standardization to a given column *)
    val normalize : (data_object Seq.t) -> (data_object Seq.t)

    (* Apply min-max normalization to a given column *)
    val min_max_normalize : (data_object Seq.t) -> (data_object Seq.t)

    (* Imputes NULL values with mean of the sequence *)
    val imputena : (data_object Seq.t) -> (data_object Seq.t)
end

module Int_transformations : INT_TRANSFORMATIONS