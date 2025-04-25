open Datatypes
open Dataframe
open Data_object
open Row

module type LIB_UTILS = 
sig
    val convert : int -> (data_object Seq.t) -> (data_object Seq.t) -> (Row.t Seq.t) -> (Row.t Seq.node)
    val filter_rows : int -> (data_object Seq.t) -> (data_object Seq.t) -> (Row.t Seq.t) -> (Row.t Seq.node)
end

module Lib_utils : LIB_UTILS = 
struct

    let rec convert col_idx mapped_column original_column original_row = 
                    
        match mapped_column () with 
        | Seq.Nil -> Seq.Nil
        | Seq.Cons(h', t') -> 
            
            match original_column () with
            | Seq.Nil -> failwith "Unexpected error in Lib.map"
            | Seq.Cons(h, t) ->

                match original_row () with
                | Seq.Nil -> failwith "Unexpected error in Lib.map"
                | Seq.Cons(rowh, rowt) ->

                    let old_row = ref (Array.of_list rowh) in
                    !old_row.(col_idx) <- h';
                    let new_row = (Array.to_list !old_row) in

                    Seq.Cons(new_row, fun () -> convert col_idx t' t rowt)

    let rec filter_rows col_idx filtered_column original_column original_row = 

        match filtered_column () with 
        | Seq.Nil -> Seq.Nil
        | Seq.Cons(h', t') ->

            match original_column () with
            | Seq.Nil -> failwith "Unexpected error in Lib.map"
            | Seq.Cons(h, t) ->

                match original_row () with
                | Seq.Nil -> failwith "Unexpected error in Lib.map"
                | Seq.Cons(rowh, rowt) ->

                    if h = h' then

                        let old_row = ref (Array.of_list rowh) in
                        !old_row.(col_idx) <- h';
                        let new_row = (Array.to_list !old_row) in

                        Seq.Cons(new_row, fun () -> filter_rows col_idx t' t rowt)

                    else 
                        filter_rows col_idx filtered_column t rowt
    
end