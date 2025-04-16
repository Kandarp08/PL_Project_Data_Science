(* Common transformations that can be applied on a column *)
module type INT_TRANSFORMATIONS = 
sig
    val normalize : (int Seq.t) -> (float Seq.t)
end
