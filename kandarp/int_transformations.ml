module type INT_TRANSFORMATIONS = 
sig
    val normalize : (int Seq.node) -> (float Seq.t)
end

module Int_Transformations : INT_TRANSFORMATIONS = 
struct
    
    let normalize node = 

        let mean = Int_Util.mean node and
        stddev = Int_Util.stddev node in

        Operations.map (fun x -> ((float_of_int x) -. mean) /. stddev) node
end