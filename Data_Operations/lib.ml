open Datatypes
open Dataframe
open Data_object
open Row

open Int_util
open Float_util
open Operations
open Int_transformations
open Float_transformations

module type LIB =
sig
    val map : Dataframe.t -> (data_object -> data_object) -> string -> Dataframe.t
end

module Lib : LIB = 
struct

    let map df f col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in 

        let mapped_column = Operations.map f column in 

        let rec convert mapped_column original_column original_row = 
            
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

                        Seq.Cons(new_row, fun () -> convert t' t rowt) in 
        
        let new_df = { df with rows = fun () -> convert mapped_column column df.rows} in
        
        new_df
end