open Datatypes
open Dataframe
open Data_object
open Row

open Int_util
open Float_util
open Operations
open Int_transformations
open Float_transformations
open Lib_utils

module type LIB =
sig
    val map : Dataframe.t -> (data_object -> data_object) -> string -> Dataframe.t
    val filter : Dataframe.t -> (data_object -> bool) -> string -> Dataframe.t
    val mem : Dataframe.t -> string -> data_object -> bool
    val fold_left : Dataframe.t -> string -> (data_object -> data_object -> data_object) -> data_object -> data_object
    val fold_right : Dataframe.t -> string -> (data_object -> data_object -> data_object) -> data_object -> data_object
    val normalize : Dataframe.t -> string -> Dataframe.t
    val fillna : Dataframe.t -> string -> Dataframe.t
end

module Lib : LIB = 
struct

    let map df f col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in 

        let mapped_column = Operations.map f column in
        
        let new_df = { df with rows = fun () -> Lib_utils.convert col_idx mapped_column column df.rows} in
        
        new_df

    let filter df f col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in 

        let filtered_column = Operations.filter f column in

        let new_df = { df with rows = fun () -> Lib_utils.filter_rows col_idx filtered_column column df.rows } in

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

        let new_df = { df with rows = fun () -> Lib_utils.convert col_idx transformed_column column df.rows } in
        
        new_df    
        
    let fillna df col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in 
        let datatype = List.nth df.dtypes col_idx in 

        let imputed_column = match datatype with 
                             | INT -> Int_transformations.fillna column 
                             | FLOAT -> Float_transformations.fillna column
                             | _ -> failwith "Inappropriate data type for imputing null values" in 

        let new_df = { df with rows = fun () -> Lib_utils.convert col_idx imputed_column column df.rows } in

        new_df
end