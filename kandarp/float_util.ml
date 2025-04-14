module type FLOAT_UTIL = 
sig
    val sum : (float Seq.node) -> float
    val len : (float Seq.node) -> int
    val mean : (float Seq.node) -> float
    val stddev : (float Seq.node) -> float
end

module Float_Util : FLOAT_UTIL = 
struct

    let sum node = 
        
        let rec aux node curr_sum = 
            match node with 
            Seq.Nil -> curr_sum
            | Cons(h, t) -> aux (t ()) (curr_sum +. h) in
            
        aux node 0.
        
    let len node = 

        let rec aux node curr_len = 
            match node with
            Seq.Nil -> curr_len
            | Cons(h, t) -> aux (t ()) (curr_len + 1) in

        aux node 0

    let mean node = 

        let float_sum = sum node and 
        float_len = float_of_int (len node) in

        float_sum /. float_len

    let stddev node =

        let rec sum_of_squares node mean curr_sum = 
            match node with 
            Seq.Nil -> curr_sum
            | Cons(h, t) -> sum_of_squares (t ()) mean (curr_sum +. ((h -. mean) *. 
                                                            (h -. mean))) in

        let length = float_of_int (len node) and
        avg =  mean node in
        let float_sum_of_squares = sum_of_squares node avg 0. in
        
        float_sum_of_squares /. length
end