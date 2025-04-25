open Data_object
open Data_object.DataObject

open Int_util
open Operations

(* Common transformations that can be applied on a column *)
module type INT_TRANSFORMATIONS = 
sig
    val normalize : (data_object Seq.t) -> (data_object Seq.t)
    val fillna : (data_object Seq.t) -> (data_object Seq.t)
end

module Int_transformations : INT_TRANSFORMATIONS