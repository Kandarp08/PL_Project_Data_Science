open Data_object
open Data_object.DataObject
open Operations

(* Utility functions to work with float sequences *)
module type FLOAT_UTIL = 
sig

    (* Sum of all elements of a sequence *)
    val sum : (data_object Seq.t) -> float

    (* Length of a sequence *)
    val len : (data_object Seq.t) -> int

    (* Mean of a sequence *)
    val mean : (data_object Seq.t) -> float

    (* Standard deviation of a sequence *)
    val stddev : (data_object Seq.t) -> float
end

module Float_util : FLOAT_UTIL