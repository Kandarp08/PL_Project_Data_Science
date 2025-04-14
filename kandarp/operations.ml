(* Contains the signatures of core data operations *)
module type OPERATIONS = 
sig
    val map : ('a -> 'b) -> 'a Seq.node -> 'b Seq.t
    val filter : ('a -> bool) -> 'a Seq.node -> 'a Seq.t

    (* Rest of the functions can also be added here *)

end

(* Module that implements the core data operations *)
module Operations : OPERATIONS =
struct
    
    (* Applies the function f on all values of the sequence *)
    let map f node = 

        (* Helper function *)
        let rec aux f node acc = 
            match node with
            Seq.Nil -> acc
            | Cons(h, t) -> aux (f) (t ()) (Seq.append acc (Seq.return (f h))) in

        aux f node (fun () -> Seq.Nil)
    
    (* Filters elements e from a sequence such that (f e) = true *)
    let filter f node = 

        (* Helper function *)
        let rec aux f node acc = 
            match node with
            Seq.Nil -> acc
            | Cons(h, t) -> if ((f h) = true) then (aux (f) (t ()) (Seq.append acc (Seq.return h)))
                            else (aux (f) (t ()) acc) in
                        
        aux f node (fun () -> Seq.Nil)
end