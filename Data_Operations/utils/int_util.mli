open Data_object
open Data_object.DataObject
open Operations

(* Utility functions to work with integer sequences *)
module type INT_UTIL = 
sig

    (* Sum of all elements of a sequence *)
    val sum : (data_object Seq.t) -> int

    (* Length of a sequence *)
    val len : (data_object Seq.t) -> int

    (* Mean of a sequence *)
    val mean : (data_object Seq.t) -> float

    (* Standard deviation of a sequence *)
    val stddev : (data_object Seq.t) -> float
end

module Int_util : INT_UTIL