(* Utility functions to work with integer sequences *)
module type INT_UTIL = 
sig

    (* Sum of all elements of a sequence *)
    val sum : (int Seq.t) -> int

    (* Length of a sequence *)
    val len : (int Seq.t) -> int

    (* Mean of a sequence *)
    val mean : (int Seq.t) -> float

    (* Standard deviation of a sequence *)
    val stddev : (int Seq.t) -> float
end

module Int_util : INT_UTIL