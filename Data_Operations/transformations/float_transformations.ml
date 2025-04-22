open Data_object
open Data_object.DataObject

open Float_util
open Operations

(* Common transformations that can be applied on a column *)
module type FLOAT_TRANSFORMATIONS = 
sig
    val normalize : (data_object Seq.t) -> (data_object Seq.t)
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
end