(* Contains the signatures of core data operations *)
module type OPERATIONS = 
sig
    val map : ('a -> 'b) -> 'a Seq.t -> 'b Seq.t
    val filter : ('a -> bool) -> 'a Seq.t -> 'a Seq.t
end

(* Module that implements the core data operations *)
module Operations : OPERATIONS =
struct
    
    let map f seq = 

        (* Helper function *)
        let rec aux seq = 
            match seq () with
            Seq.Nil -> Seq.Nil
            | Seq.Cons(h, t) -> Seq.Cons(f h, fun () -> (aux t)) in

        fun () -> aux seq
    
    let filter f seq = 

        (* Helper function *)
        let rec aux seq = 
            match seq () with
            Seq.Nil -> Seq.Nil
            | Seq.Cons(h, t) -> if ((f h) = true) then Seq.Cons(h, fun () -> (aux t))
                                else (aux t) in
                        
        fun () -> aux seq
end