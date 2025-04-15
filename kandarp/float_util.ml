(* Utility functions to work with float sequences *)
module type FLOAT_UTIL = 
sig
    val sum : (float Seq.t) -> float
    val len : (float Seq.t) -> int
    val mean : (float Seq.t) -> float
    val stddev : (float Seq.t) -> float
end

module Float_Util : FLOAT_UTIL = 
struct

    let sum seq = 
        
        (* Helper function *)
        let rec aux seq = 
            match seq () with 
            Seq.Nil -> 0.
            | Seq.Cons(h, t) -> h +. (aux t) in
            
        aux seq
        
    let len seq = 

        (* Helper function *)
        let rec aux seq = 
            match seq () with
            Seq.Nil -> 0
            | Seq.Cons(h, t) -> 1 + (aux t)in

        aux seq

    let mean seq = 

        let float_sum = sum seq and           (* Sum of sequence *)
        float_len = float_of_int (len seq) in (* Length of sequence *)

        float_sum /. float_len

    let stddev seq =

        (* Find summation of (x - mean) ^ 2 *)
        let rec sum_of_squares seq mean = 
            match seq () with 
            Seq.Nil -> 0.
            | Seq.Cons(h, t) -> ((h -. mean) *. (h -. mean)) +. sum_of_squares t mean in

        let length = float_of_int (len seq) and
        avg =  mean seq in
        let float_sum_of_squares = sum_of_squares seq avg in
        
        float_sum_of_squares /. length
end