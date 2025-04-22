open Data_object
open Data_object.DataObject

open Float_util
open Operations

(* Common transformations that can be applied on a column *)
module type FLOAT_TRANSFORMATIONS = 
sig
    val normalize : (data_object Seq.t) -> (data_object Seq.t)
end

module Float_transformations : FLOAT_TRANSFORMATIONS