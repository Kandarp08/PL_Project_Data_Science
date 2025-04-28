open Data_object
open Data_object.DataObject
open Operations

(* Utility functions to work with float sequences *)
module type FLOAT_UTIL = 
sig
    val sum : (data_object Seq.t) -> float
    val len : (data_object Seq.t) -> int
    val mean : (data_object Seq.t) -> float
    val stddev : (data_object Seq.t) -> float
end

module Float_util : FLOAT_UTIL = 
struct

    let sum seq =

        (* Convert given sequence fo float sequence *)
        let float_seq = seq 
                        |> Operations.map to_string 
                        |> Operations.map (fun s -> from_string FLOAT s)
                        |> Operations.map (function FLOAT_DATA i -> i | _ -> 0.) in

        (* Helper function*)
        let rec aux seq curr_sum =
            match seq () with
            | Seq.Nil -> curr_sum
            | Seq.Cons(h, t) -> aux t (h +. curr_sum) in
        
        aux float_seq 0.
            
        let len seq = 

            (* Helper function *)
            let rec aux seq curr_len = 
                match seq () with
                Seq.Nil -> curr_len
                | Seq.Cons(h, t) -> aux t (1 + curr_len) in

            aux seq 0

    let mean seq = 

        let float_sum = sum seq and           (* Sum of sequence *)
        float_len = float_of_int (len seq) in (* Length of sequence *)

        float_sum /. float_len

    let stddev seq =

        (* Convert given sequence to float sequence *)
        let float_seq = seq 
                        |> Operations.map to_string 
                        |> Operations.map (fun s -> from_string FLOAT s)
                        |> Operations.map (function FLOAT_DATA i -> i | _ -> 0.) in

        (* Find summation of (x - mean) ^ 2 *)
        let rec sum_of_squares seq mean = 
            match seq () with 
            Seq.Nil -> 0.
            | Seq.Cons(h, t) -> ((h -. mean) *. (h -. mean)) +. sum_of_squares t mean in

        let length = float_of_int (len seq) and
        avg =  mean seq in
        let float_sum_of_squares = sum_of_squares float_seq avg in
        
        Float.sqrt (float_sum_of_squares /. length)
end