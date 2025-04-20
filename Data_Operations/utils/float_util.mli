(* Utility functions to work with float sequences *)
module type FLOAT_UTIL = 
sig

    (* Sum of all elements of a sequence *)
    val sum : (float Seq.t) -> float

    (* Length of a sequence *)
    val len : (float Seq.t) -> int

    (* Mean of a sequence *)
    val mean : (float Seq.t) -> float

    (* Standard deviation of a sequence *)
    val stddev : (float Seq.t) -> float
end

module Float_util : FLOAT_UTIL