(* Contains the signatures of core data operations *)
module type OPERATIONS = 
sig
    val map : ('a -> 'b) -> 'a list -> 'b list
    val filter : ('a -> bool) -> 'a list -> 'a list

    (* Rest of the functions can also be added here *)

end

(* Module that implements the core data operations *)
module Operations : OPERATIONS =
struct
    
    (* Applies the function f on all values of the list l *)
    let map f l = 

        (* Helper function *)
        let rec aux f l acc = 
            match l with
            [] -> acc
            | h :: t -> aux (f) t (acc @ [f h]) in

        aux f l []
    
    (* Filters elements e from a list l such that (f e) = true *)
    let filter f l = 

        (* Helper function *)
        let rec aux f l acc = 
            match l with
            [] -> acc
            | h :: t -> if ((f h) = true) then (aux (f) t (acc @ [h]))
                        else (aux (f) t acc) in
                        
        aux f l []
end