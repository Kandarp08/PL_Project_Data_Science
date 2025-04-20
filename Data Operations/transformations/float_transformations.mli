(* Common transformations that can be applied on a column *)
module type FLOAT_TRANSFORMATIONS = 
sig
    val normalize : (float Seq.t) -> (float Seq.t)
end

module Float_transformations : FLOAT_TRANSFORMATIONS