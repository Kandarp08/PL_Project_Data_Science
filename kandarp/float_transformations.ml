module type FLOAT_TRANSFORMATIONS = 
sig
    val normalize : (float list) -> (float list)
end

module Float_Transformations : FLOAT_TRANSFORMATIONS = 
struct
    
    let normalize l = 

        let mean = Float_Util.mean l and
        stddev = Float_Util.stddev l in

        Operations.map (fun x -> (x -. mean) /. stddev) l
end