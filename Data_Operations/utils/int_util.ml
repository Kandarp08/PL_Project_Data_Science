open Data_object
open Data_object.DataObject
open Operations

(* Utility functions to work with integer sequences *)
module type INT_UTIL = 
sig
    val sum : (data_object Seq.t) -> int
    val len : (data_object Seq.t) -> int
    val mean : (data_object Seq.t) -> float
    val stddev : (data_object Seq.t) -> float
end

module Int_util : INT_UTIL = 
struct

    let sum seq =

        (* Converting given sequence to integer sequence *)
        let int_seq = seq 
                        |> Operations.map to_string 
                        |> Operations.map (fun s -> from_string INT s)
                        |> Operations.map (function INT_DATA i -> i | _ -> 0) in

        (* Helper function*)
        let rec aux seq =
            match seq () with
            | Seq.Nil -> 0
            | Seq.Cons(h, t) -> h + aux t in
        
        aux int_seq
        
    let len seq = 

        (* Helper function *)
        let rec aux seq = 
            match seq () with
            Seq.Nil -> 0
            | Seq.Cons(h, t) -> 1 + (aux t)in

        aux seq

    let mean seq = 

        let float_sum = float_of_int (sum seq) and (* Sum of all elements *) 
        float_len = float_of_int (len seq) in      (* Length of sequence *)

        float_sum /. float_len

    let stddev seq =

        (* Converting given sequence to integer sequence *)
        let int_seq = seq 
                        |> Operations.map to_string 
                        |> Operations.map (fun s -> from_string INT s)
                        |> Operations.map (function INT_DATA i -> i | _ -> 0) in

        (* Find summation of (x - mean) ^ 2 *)
        let rec sum_of_squares seq mean = 
            match seq () with 
            Seq.Nil -> 0.
            | Seq.Cons(h, t) -> (((float_of_int h) -. mean) *. ((float_of_int h) -. mean)) +. sum_of_squares t mean in

        let length = float_of_int (len seq) and
        avg =  mean seq in
        let float_sum_of_squares = sum_of_squares int_seq avg in 
        
        Float.sqrt (float_sum_of_squares /. length)
end