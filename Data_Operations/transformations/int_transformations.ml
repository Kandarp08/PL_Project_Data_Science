open Int_util
open Operations

(* Common transformations that can be applied on a column *)
module type INT_TRANSFORMATIONS = 
sig
    val normalize : (int Seq.t) -> (float Seq.t)
end

module Int_transformations : INT_TRANSFORMATIONS = 
struct
    
    let normalize seq = 

        let mean = Int_util.mean seq and (* Mean of sequence *)
        stddev = Int_util.stddev seq in  (* Standard deviation of sequence *)

        (* Use the map function to carry out normalization *)
        Operations.map (fun x -> ((float_of_int x) -. mean) /. stddev) seq
end