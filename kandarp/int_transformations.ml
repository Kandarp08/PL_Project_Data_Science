module type INT_TRANSFORMATIONS = 
sig
    val normalize : (int list) -> (float list)
end

module Int_Transformations : INT_TRANSFORMATIONS = 
struct
    
    let normalize l = 

        let mean = Int_Util.mean l and
        stddev = Int_Util.stddev l in

        Operations.map (fun x -> ((float_of_int x) -. mean) /. stddev) l
end