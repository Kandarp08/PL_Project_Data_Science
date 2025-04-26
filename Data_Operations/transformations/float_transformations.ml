open Data_object
open Data_object.DataObject

open Float_util
open Operations

(* Common transformations that can be applied on a column *)
module type FLOAT_TRANSFORMATIONS = 
sig
    val normalize : (data_object Seq.t) -> (data_object Seq.t)
    val min_max_normalize : (data_object Seq.t) -> (data_object Seq.t)
    val fillna : (data_object Seq.t) -> (data_object Seq.t)
end

module Float_transformations : FLOAT_TRANSFORMATIONS = 
struct
    
    let normalize seq = 

        let mean = Float_util.mean seq and (* Mean of sequence *)
        stddev = Float_util.stddev seq in  (* Standard deviation of sequence *)

        let normalization_function x = to_string x
                                    |> from_string FLOAT
                                    |> (function FLOAT_DATA x -> (x -. mean) /. stddev | _ -> 0.)
                                    |> string_of_float
                                    |> from_string FLOAT in

        (* Use the map function to carry out normalization *)
        Operations.map normalization_function seq 

    let fillna seq = 

        let is_not_null x =

            match to_string x with
            | "NULL" -> false
            | _ -> true

        and 

        impute_null impute_val x = 

            match float_of_string_opt (to_string x) with 
            | Some n -> FLOAT_DATA (n)
            | None -> FLOAT_DATA (impute_val)

        in

        let non_null = Operations.filter is_not_null seq in 
        let mean = Float_util.mean non_null in 

        Operations.map (impute_null mean) seq

    let min_max_normalize seq = 

        let rec min seq mn = 

            match seq () with
            | Seq.Nil -> mn
            | Seq.Cons(h, t) -> 
                match h with 
                | FLOAT_DATA x -> if x < mn then min t x else min t mn
                | _ -> failwith "Unexpected error in min_max_normalize" in

        let rec max seq mx = 
            
            match seq () with
            | Seq.Nil -> mx
            | Seq.Cons(h, t) ->
                match h with 
                | FLOAT_DATA x -> if x > mx then max t x else max t mx
                | _ -> failwith "Unexpected error in min_max_normalize" in

        let mn = min seq max_float and
        mx = max seq min_float in

        let normalization_function x = to_string x
                                    |> from_string FLOAT
                                    |> (function FLOAT_DATA x -> (x -. mn) /. (mx -. mn) | _ -> 0.)
                                    |> string_of_float
                                    |> from_string FLOAT in

        (* Use the map function to carry out normalization *)
        Operations.map normalization_function seq
end