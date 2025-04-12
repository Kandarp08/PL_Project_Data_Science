module type TRANSFORMATIONS = 
sig
    val normalize : (int list) -> (float list)
end

module Transformations : TRANSFORMATIONS = 
struct
    
    let normalize l = 

        let mean = Util.mean l and
        stddev = Util.stddev l in

        Operations.map (fun x -> ((float_of_int x) -. mean) /. stddev) l
end