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
    val min_max_normalize : Dataframe.t -> string -> Dataframe.t
    val imputena : Dataframe.t -> string -> Dataframe.t
    val fillna : Dataframe.t -> string -> data_object -> Dataframe.t
    val join : Dataframe.t -> Dataframe.t -> string -> Dataframe.t
    val sum : Dataframe.t -> string -> data_object
    val len : Dataframe.t -> string -> int
    val mean : Dataframe.t -> string -> float
    val stddev : Dataframe.t -> string -> float
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
        
    let min_max_normalize df col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        let transformed_column = match datatype with
                                | INT -> Int_transformations.min_max_normalize column 
                                | FLOAT -> Float_transformations.min_max_normalize column
                                | _ -> failwith "Inappropriate data type for normalization" in

        let new_df = { df with rows = fun () -> Lib_utils.convert col_idx transformed_column column df.rows } in
        
        new_df
        
    let imputena df col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in 
        let datatype = List.nth df.dtypes col_idx in 

        let imputed_column = match datatype with 
                             | INT -> Int_transformations.fillna column 
                             | FLOAT -> Float_transformations.fillna column
                             | _ -> failwith "Inappropriate data type for imputing null values" in 

        let new_df = { df with rows = fun () -> Lib_utils.convert col_idx imputed_column column df.rows } in

        new_df

    let fillna df col_name el = 

        let col_idx = Dataframe.get_column_index df col_name in
        let datatype = List.nth df.dtypes col_idx and
        column = Dataframe.get_column df col_name and

        fill x = 
            match Data_object.DataObject.to_string x with
            | "NULL" -> el
            | _ -> x in

        if (Data_object.DataObject.get_datatype el) <> datatype then failwith "Value to be inserted does not match column's datatype";

        let filled_column = Operations.map fill column in 

        let new_df = { df with rows = fun () -> Lib_utils.convert col_idx filled_column column df.rows } in

        new_df

    let rec join df1 df2 colName =
        let l1 = Lib_utils.get_rows_as_list df1 in
        let l2 = Lib_utils.get_rows_as_list df2 in

        let rec joinHelper (l1: 'a list) (l2: 'a list) colName = 
            match (l1, l2) with
            | ([], _) -> []
            | (_, []) -> []
            | (l1_hd::l1_tl, l2_hd::l2_tl) -> 
            let list_including_emptyLists = (Lib_utils.joinItemWithList l1_hd l2 colName df1 df2) @ (joinHelper l1_tl l2 colName) in
            List.filter (fun subList -> subList <> []) list_including_emptyLists
        in
        let finalJoinedList = joinHelper l1 l2 colName in
        
        Lib_utils.convertToDataFrame finalJoinedList df1 df2

    let sum df col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        match datatype with
        | INT -> INT_DATA (Int_util.sum column)
        | FLOAT -> FLOAT_DATA (Float_util.sum column)
        | _ -> failwith "Inappropriate datatype for sum"

    let len df col_name = 

        let column = Dataframe.get_column df col_name in  
        
        let rec aux seq = 
            match seq () with
                Seq.Nil -> 0
                | Seq.Cons(h, t) -> 1 + (aux t) in

        aux column

    let mean df col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        match datatype with
        | INT -> Int_util.mean column
        | FLOAT -> Float_util.mean column
        | _ -> failwith "Inappropriate datatype for mean"

    
    let stddev df col_name = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        match datatype with
        | INT -> Int_util.stddev column
        | FLOAT -> Float_util.stddev column
        | _ -> failwith "Inappropriate datatype for stddev"
end