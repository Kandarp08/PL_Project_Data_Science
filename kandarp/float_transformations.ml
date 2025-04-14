module type FLOAT_TRANSFORMATIONS = 
sig
    val normalize : (float Seq.node) -> (float Seq.t)
end

module Float_Transformations : FLOAT_TRANSFORMATIONS = 
struct
    
    let normalize node = 

        let mean = Float_Util.mean node and
        stddev = Float_Util.stddev node in

        Operations.map (fun x -> (x -. mean) /. stddev) node
end