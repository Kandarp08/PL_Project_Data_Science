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
    val show_df : Dataframe.t -> unit
    val map : (data_object -> data_object) -> string -> Dataframe.t -> Dataframe.t
    val filter : (data_object -> bool) -> string -> Dataframe.t -> Dataframe.t
    val mem : string -> data_object -> Dataframe.t -> bool
    val fold_left : string -> (data_object -> data_object -> data_object) -> data_object -> Dataframe.t -> data_object
    val fold_right : string -> (data_object -> data_object -> data_object) -> data_object -> Dataframe.t -> data_object
    val normalize : string -> Dataframe.t -> Dataframe.t
    val min_max_normalize : string -> Dataframe.t -> Dataframe.t
    val imputena : string -> Dataframe.t -> Dataframe.t
    val fillna : string -> data_object -> Dataframe.t -> Dataframe.t
    val join : Dataframe.t -> Dataframe.t -> string -> Dataframe.t
    val sum : string -> Dataframe.t -> data_object
    val len : string -> Dataframe.t -> data_object
    val mean : string -> Dataframe.t -> data_object
    val stddev : string -> Dataframe.t -> data_object
    val add_row : Row.t -> Dataframe.t -> Dataframe.t
    val delete_row : (Row.t -> bool) -> Dataframe.t -> Dataframe.t
    val groupByAggregate : string -> (string * (string -> Dataframe.t -> data_object)) list -> Dataframe.t -> Dataframe.t
end

module Lib : LIB = 
struct

    let show_df df = Seq.iter Row.display_row df.rows

    let map f col_name df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let datatype = List.nth df.dtypes col_idx in
        let column = Dataframe.get_column df col_name in 

        let mapped_column = Operations.map f column in (* New column values *)

        (* New datatype of the column *)
        let new_datatype = match mapped_column () with 
                            | Seq.Nil -> datatype
                            | Seq.Cons(h, _) -> Data_object.DataObject.get_datatype h in

        (* Update the datatype of the column *)
        let dtypes = Array.of_list df.dtypes in 
        dtypes.(col_idx) <- new_datatype;
        let new_dtypes = Array.to_list dtypes in
    
        let new_df = { df with 
                        rows = (fun () -> Lib_utils.convert col_idx mapped_column column df.rows);
                        dtypes = new_dtypes;    
                    } in
        
        new_df

    let filter f col_name df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in 

        let filtered_column = Operations.filter f column in (* Filtered column *)

        let new_df = { df with rows = fun () -> Lib_utils.filter_rows col_idx filtered_column column df.rows } in

        new_df

    let mem col_name el df = 

        let column = Dataframe.get_column df col_name in

        Operations.mem el column

    let fold_left col_name f init df = 

        let column = Dataframe.get_column df col_name in

        Operations.fold_left f init column

    let fold_right col_name f init df = 

        let column = Dataframe.get_column df col_name in

        Operations.fold_right f column init

    let normalize col_name df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        (* Transformed column *)
        let transformed_column = match datatype with
                                | INT -> Int_transformations.normalize column 
                                | FLOAT -> Float_transformations.normalize column
                                | _ -> failwith "Inappropriate data type for normalization" in

        (* New datatype of column *)
        let new_dtypes = 
            let dtypes = Array.of_list df.dtypes in
            
            match dtypes.(col_idx) with 
            | FLOAT -> df.dtypes
            | INT -> (dtypes.(col_idx) <- FLOAT; Array.to_list dtypes)
            | _ -> failwith "Inappropriate data type for normalization" in
        
        let new_df = { df with 
                        rows = (fun () -> Lib_utils.convert col_idx transformed_column column df.rows);
                        dtypes = new_dtypes;    
                    } in
        
        new_df


    let min_max_normalize col_name df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        (* Transformed column *)
        let transformed_column = match datatype with
                                | INT -> Int_transformations.min_max_normalize column 
                                | FLOAT -> Float_transformations.min_max_normalize column
                                | _ -> failwith "Inappropriate data type for normalization" in

        (* New datatype of column *)
        let new_dtypes = 
            let dtypes = Array.of_list df.dtypes in
            
            match dtypes.(col_idx) with 
            | FLOAT -> df.dtypes
            | INT -> (dtypes.(col_idx) <- FLOAT; Array.to_list dtypes)
            | _ -> failwith "Inappropriate data type for normalization" in
        
        let new_df = { df with 
                        rows = (fun () -> Lib_utils.convert col_idx transformed_column column df.rows);
                        dtypes = new_dtypes;    
                    } in
        
        new_df

    let imputena col_name df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in 
        let datatype = List.nth df.dtypes col_idx in 

        (* Column with NULL values imputed *)
        let imputed_column = match datatype with 
                             | INT -> Int_transformations.imputena column 
                             | FLOAT -> Float_transformations.imputena column
                             | _ -> failwith "Inappropriate data type for imputing null values" in 

        let new_df = { df with 
                        rows = (fun () -> Lib_utils.convert col_idx imputed_column column df.rows);   
                    } in
        
        new_df

    let fillna col_name el df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let datatype = List.nth df.dtypes col_idx and
        column = Dataframe.get_column df col_name and

        (* Function to replace NULL values with given value el *)
        fill x = 
            match Data_object.DataObject.to_string x with
            | "NULL" -> el
            | _ -> x in

        (* If datatypes do not match *)
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

    let sum col_name df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        match datatype with
        | INT -> INT_DATA (Int_util.sum column)
        | FLOAT -> FLOAT_DATA (Float_util.sum column)
        | _ -> failwith "Inappropriate datatype for sum"

    let len col_name df = 

        let column = Dataframe.get_column df col_name in  
        
        let rec aux seq curr_len = 
            match seq () with
                Seq.Nil -> curr_len
                | Seq.Cons(h, t) -> aux t (curr_len + 1) in

        INT_DATA (aux column 0)

    let mean col_name df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        match datatype with
        | INT -> FLOAT_DATA (Int_util.mean column)
        | FLOAT -> FLOAT_DATA (Float_util.mean column)
        | _ -> failwith "Inappropriate datatype for mean"

    
    let stddev col_name df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        match datatype with
        | INT -> FLOAT_DATA (Int_util.stddev column)
        | FLOAT -> FLOAT_DATA (Float_util.stddev column)
        | _ -> failwith "Inappropriate datatype for stddev"

    let add_row row df = 

        (* Check whether data types of new row elements are appropriate or not *)
        let rec validate_row row dtypes = 

            match (row, dtypes) with 
            | ([], []) -> true
            | ([], _) -> false
            | (_, []) -> false
            | (obj :: objt, dt :: dtt) ->

                if (Data_object.DataObject.get_datatype obj) <> dt then false
                else validate_row objt dtt in

        let validation = validate_row row df.dtypes in

        match validation with

        | false -> failwith "Data types of new row are incompatible"
        | true -> { df with rows = fun () -> Seq.Cons(row, df.rows) }

    let delete_row f df = 

        let rec aux row = 

            match row () with 
            | Seq.Nil -> Seq.Nil
            | Seq.Cons(curr_row, rowt) ->

                (* Delete row if function f returns true *)
                if f curr_row then aux rowt
                else Seq.Cons(curr_row, fun () -> aux rowt) in

        let new_df = { df with rows = fun () -> aux df.rows } in

        new_df

    let groupByAggregate colName colToFnMapping df = 
        let uniqueValues = (Lib_utils.getUniqueValues df colName) in

        let groupedRows = Operations.map (Lib_utils.createRow df colName colToFnMapping) uniqueValues in
        Lib_utils.convertRowsToDataframe df groupedRows
end