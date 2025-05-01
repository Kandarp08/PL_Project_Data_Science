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
    val update_row : (Row.t -> bool) -> (Row.t -> Row.t) -> Dataframe.t -> Dataframe.t
    val groupByAggregate : string -> (string * (string -> Dataframe.t -> data_object)) list -> Dataframe.t -> Dataframe.t
    val iloc : int -> int -> Dataframe.t -> Dataframe.t
    val loc : string -> string -> string -> Dataframe.t -> Dataframe.t
end

module Lib : LIB = 
struct

    (* Displays the dataframe contents in a formatted tabular representation.
   Shows column headers, data rows, and a summary of dimensions.
   If dataframe is empty, shows "Empty dataframe" message.
   Uses compute_column_widths and print_separator for formatting. *)
    let show_df df = 

        (* If the dataframe is empty, show a message *)
        if df.headers = [] then
            print_endline "Empty dataframe"
        else
            let col_widths = Lib_utils.compute_column_widths df in
            
            (* Print the header row *)
            Lib_utils.print_separator col_widths;
            print_string "| ";
            List.iter2 (fun header width ->
            Printf.printf "%-*s | " width header
            ) df.headers col_widths;
            print_newline ();
            Lib_utils.print_separator col_widths;
            
            (* Print each data row *)
            let rows_list = Lib_utils.get_rows_as_list df in
            List.iter (fun row ->
            print_string "| ";
            List.iteri (fun i cell ->
                if i < List.length col_widths then
                let width = List.nth col_widths i in
                Printf.printf "%-*s | " width (Lib_utils.string_of_data_object cell)
            ) row;
            print_newline ()
            ) rows_list;
            Lib_utils.print_separator col_widths;
            
            (* Print dataframe summary *)
            Printf.printf "Dataframe with %d columns and %d rows\n" 
            df.ncols (List.length rows_list)
            
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
        let new_dtypes = Lib_utils.update_pos df.dtypes new_datatype col_idx 0 in
    
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

    (* Checks if an element exists in a specified column of the dataframe.
   Returns true if element is found, false otherwise.
   Delegates to Operations.mem for the actual membership check. *)
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

        (* Update the datatype of the column *)
        let new_dtypes = Lib_utils.update_pos df.dtypes FLOAT col_idx 0 in
        
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

        (* Update the datatype of the column *)
        let new_dtypes = Lib_utils.update_pos df.dtypes FLOAT col_idx 0 in
        
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

    (* Joins two dataframes based on matching values in a specified column.
   Similar to SQL inner join operation.
   Returns a new dataframe with combined schema and joined rows.
   Filters out empty lists (non-matches) from the results. *)
    let rec join df1 df2 colName =
        let l1 = df1.rows in
        let l2 = df2.rows in

        let rec joinHelper l1 l2 colName = 
            match (l1 (), l2 ()) with
            | (Seq.Nil, _) -> Seq.empty
            | (_, Seq.Nil) -> Seq.empty
            | (Seq.Cons(l1_hd, l1_tl), Seq.Cons(l2_hd, l2_tl)) -> 
                (* Seq.append (Lib_utils.joinItemWithList l1_hd l2 colName df1 df2) (joinHelper l1_tl l2 colName) in *)
            let list_including_emptyLists = Seq.append (Lib_utils.joinItemWithList l1_hd l2 colName df1 df2) (joinHelper l1_tl l2 colName) in
            Operations.filter (fun subList -> subList <> []) list_including_emptyLists in
        
        let finalJoinedList = joinHelper l1 l2 colName in
        
        Lib_utils.convertToDataFrame finalJoinedList df1 df2

    (* Calculates the sum of values in a numeric column.
   Only works with INT or FLOAT columns.
   Returns the result as the appropriate data_object type. *)
    let sum col_name df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        match datatype with
        | INT -> INT_DATA (Int_util.sum column)
        | FLOAT -> FLOAT_DATA (Float_util.sum column)
        | _ -> failwith "Inappropriate datatype for sum"

    (* Counts the number of elements in a specified column.
   Returns the count as an INT_DATA value.
   Uses a recursive auxiliary function to perform the counting. *)
    let len col_name df = 

        let column = Dataframe.get_column df col_name in  
        
        let rec aux seq curr_len = 
            match seq () with
                Seq.Nil -> curr_len
                | Seq.Cons(h, t) -> aux t (curr_len + 1) in

        INT_DATA (aux column 0)

    (* Calculates the mean of values in a numeric column.
   Only works with INT or FLOAT columns.
   Returns the result as a FLOAT_DATA value. *)
    let mean col_name df = 

        let col_idx = Dataframe.get_column_index df col_name in
        let column = Dataframe.get_column df col_name in  
        let datatype = List.nth df.dtypes col_idx in 

        match datatype with
        | INT -> FLOAT_DATA (Int_util.mean column)
        | FLOAT -> FLOAT_DATA (Float_util.mean column)
        | _ -> failwith "Inappropriate datatype for mean"

    (* Calculates the standard deviation of values in a numeric column.
   Only works with INT or FLOAT columns.
   Returns the result as a FLOAT_DATA value. *)
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

    let update_row f update df = 

        (* Check whether data types of new row elements are appropriate or not *)
        let rec validate_row row dtypes = 

            match (row, dtypes) with 
            | ([], []) -> true
            | ([], _) -> false
            | (_, []) -> false
            | (obj :: objt, dt :: dtt) ->

                if (Data_object.DataObject.get_datatype obj) <> dt then false
                else validate_row objt dtt in

        let rec aux row = 

            match row () with
            | Seq.Nil -> Seq.Nil
            | Seq.Cons(curr_row, rowt) ->

                if f curr_row then 

                    let updated_row = update curr_row in 

                    if validate_row updated_row df.dtypes then
                        Seq.Cons(updated_row, fun () -> aux rowt)

                    else failwith "Data types of updated row do not match the data types of dataframe"
                
                else Seq.Cons(curr_row, fun () -> aux rowt) in

        let new_df = { df with rows = fun () -> aux df.rows } in

        new_df

    (* Groups data by unique values in a specified column and applies aggregation functions.
   Gets unique values in the grouping column.
   Creates new rows by applying aggregation functions to each group.
   Returns a new dataframe with the aggregated results. *)
    let groupByAggregate colName colToFnMapping df = 
        let uniqueValues = (Lib_utils.getUniqueValues df colName) in

        let groupedRows = Operations.map (Lib_utils.createRow df colName colToFnMapping) uniqueValues in
        Lib_utils.convertRowsToDataframe df groupedRows

    (* Extracts rows from the dataframe based on integer indices.
   Returns a new dataframe with rows from startIndex to endIndex inclusive.
   Throws errors for invalid indices or out-of-bounds conditions.
   Uses iloc_helper to perform the extraction. *)
    let iloc startIndex endIndex df = 
        if startIndex < 0 then failwith "startIndex cannot be negative"
        else if endIndex < 0 then failwith "endIndex cannot be negative"  
        else if startIndex > endIndex then failwith "startIndex must be less than or equal to endIndex"
        else
            let (reachedEnd, outputRows) = Lib_utils.iloc_helper df.rows startIndex endIndex 0 Seq.empty false in
            if not reachedEnd then
            failwith (Printf.sprintf "endIndex %d is out of bounds for sequence" endIndex)
            else
            Lib_utils.convertRowsToDataframe df outputRows

    (* Similar to iloc but for dataframeLoc type.
   Internal function used by loc to extract rows by index after label lookup. *)
    let iloc_loc startIndex endIndex df = 
        if startIndex < 0 then failwith "startIndex cannot be negative"
        else if endIndex < 0 then failwith "endIndex cannot be negative"  
        else if startIndex > endIndex then failwith "startIndex must be less than or equal to endIndex"
        else
            let (reachedEnd, outputRows) = Lib_utils.iloc_helper df.lrows startIndex endIndex 0 Seq.empty false in
            if not reachedEnd then
            failwith (Printf.sprintf "endIndex %d is out of bounds for sequence" endIndex)
            else
            Lib_utils.convertRowsToDataframeLoc df outputRows

    (* Extracts rows from the dataframe based on labels in an indexed column.
   First creates an indexed dataframe using the specified column.
   Looks up indices for the start and end labels.
   Uses iloc_loc to extract the rows based on these indices.
   Throws error if labels not found or index not set. *)
    let loc colName startLabel endLabel df = 

        let indexed_df = Lib_utils.set_index colName df in

        match indexed_df.lindices with
        | None -> failwith "Dataframe has no index set"
        | Some lindices ->
            try
                let start_idx = Hashtbl.find lindices startLabel in
                let end_idx = Hashtbl.find lindices endLabel in

                iloc_loc start_idx end_idx indexed_df  (* REUSING MY EXISTING FUNCTION *)
                with Not_found ->
                failwith "One or more labels not found in index"
end
