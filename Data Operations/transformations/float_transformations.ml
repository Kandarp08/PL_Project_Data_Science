open Float_util
open Operations

module type FLOAT_TRANSFORMATIONS = 
sig
    val normalize : (float Seq.t) -> (float Seq.t)
end

module Float_transformations : FLOAT_TRANSFORMATIONS = 
struct
    
    let normalize seq = 

        let mean = Float_util.mean seq and (* Mean of sequence *)
        stddev = Float_util.stddev seq in  (* Standard deviation of sequence *)
        
        (* Use the map function to carry out normalization *)
        Operations.map (fun x -> (x -. mean) /. stddev) seq
end