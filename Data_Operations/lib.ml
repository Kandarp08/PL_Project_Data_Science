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
    val filter : Dataframe.t -> (data_object -> bool) -> string -> Dataframe.t
    val mem : Dataframe.t -> string -> data_object -> bool
    val fold_left : Dataframe.t -> string -> (data_object -> data_object -> data_object) -> data_object -> data_object
    val fold_right : Dataframe.t -> string -> (data_object -> data_object -> data_object) -> data_object -> data_object
    val normalize : Dataframe.t -> string -> Dataframe.t
end

module Lib : LIB = 
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

    let map df f col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in 

        let mapped_column = Operations.map f column in
        
        let new_df = { df with rows = fun () -> convert col_idx mapped_column column df.rows} in
        
        new_df

    let filter df f col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in 

        let filtered_column = Operations.filter f column in

        let new_df = { df with rows = fun () -> filter_rows col_idx filtered_column column df.rows } in

        new_df

    let mem df col_name el = 

        let column = Dataframe.get_column df col_name in

        Operations.mem el column

    let fold_left df col_name f init = 

        let column = Dataframe.get_column df col_name in

        Operations.fold_left f init column

    let fold_right df col_name f init = 

        let column = Dataframe.get_column df col_name in

        Operations.fold_right f column init

    let normalize df col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        let transformed_column = match datatype with
                                | INT -> Int_transformations.normalize column 
                                | FLOAT -> Float_transformations.normalize column
                                | _ -> failwith "Inappropriate data type for normalization" in

        let new_df = { df with rows = fun () -> convert col_idx transformed_column column df.rows} in
        
        new_df                
end