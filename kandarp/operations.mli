(* Contains the signatures of core data operations *)
module type OPERATIONS = 
sig

    (* Apply a given function on each element of the sequence *)
    val map : ('a -> 'b) -> 'a Seq.t -> 'b Seq.t

    (* Filter elements of a sequence which satisfy a particular function *)
    val filter : ('a -> bool) -> 'a Seq.t -> 'a Seq.t
end