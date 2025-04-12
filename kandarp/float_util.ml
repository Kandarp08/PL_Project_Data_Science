module type FLOAT_UTIL = 
sig
    val sum : (float list) -> float
    val len : (float list) -> int
    val mean : (float list) -> float
    val stddev : (float list) -> float
end

module Float_Util : FLOAT_UTIL = 
struct

    let sum l = 
        
        let rec aux l curr_sum = 
            match l with 
            [] -> curr_sum
            | h :: t -> aux t (curr_sum +. h) in
            
        aux l 0.
        
    let len l = 

        let rec aux l curr_len = 
            match l with
            [] -> curr_len
            | h :: t -> aux t (curr_len + 1) in

        aux l 0

    let mean l = 

        let float_sum = sum l and 
        float_len = float_of_int (len l) in

        float_sum /. float_len

    let stddev l =

        let rec sum_of_squares l mean curr_sum = 
            match l with 
            [] -> curr_sum
            | h :: t -> sum_of_squares t mean (curr_sum +. ((h -. mean) *. 
                                                            (h -. mean))) in

        let length = float_of_int (len l) and
        avg =  mean l in
        let float_sum_of_squares = sum_of_squares l avg 0. in
        
        float_sum_of_squares /. length
end