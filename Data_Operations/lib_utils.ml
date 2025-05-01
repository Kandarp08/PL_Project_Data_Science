open Datatypes
open Dataframe
open Data_object
open Row

open Operations
open Int_util
open Float_util

type dataframeLoc = {
    lheaders: string list;
    ldtypes: datatype list;
    lrows: Row.t Seq.t;
    lncols: int;
    lindices: (string, int) Hashtbl.t option; (* Maps row labels to row indices *)
}


module type LIB_UTILS = 
sig
    val update_pos : 'a list -> 'a -> int -> int -> 'a list
    val convert : int -> data_object Seq.t -> data_object Seq.t -> Row.t Seq.t -> Row.t Seq.node
    val filter_rows : int -> data_object Seq.t -> data_object Seq.t -> Row.t Seq.t -> Row.t Seq.node
    val removeElement_atIndex : int -> 'a list -> 'a list
    val removeField : data_object list -> string -> Dataframe.t -> data_object list
    val findValueHelper : int -> 'a list -> int -> 'a
    val findValue : string -> 'a list -> Dataframe.t -> 'a
    val mergeIntoSingleRecord : data_object list -> data_object list -> string -> Dataframe.t -> Dataframe.t -> data_object list
    val get_rows_as_list : Dataframe.t -> Row.t list
    val joinItemWithList : data_object list -> data_object list Seq.t -> string -> Dataframe.t -> Dataframe.t -> data_object list Seq.t
    val convertToDataFrame : Row.t Seq.t -> Dataframe.t -> Dataframe.t -> Dataframe.t
    val convertRowsToDataframe : Dataframe.t -> Row.t Seq.t -> Dataframe.t
    val convertRowsToDataframeLoc : dataframeLoc -> Row.t Seq.t -> Dataframe.t
    val extract_int : data_object -> int
    val extract_float : data_object -> float
    val get_int_values : data_object Seq.t -> int Seq.t
    val get_float_values : data_object Seq.t -> float Seq.t
    val seq_to_list : 'a Seq.t -> 'a list
    val singleAggregateResult : Dataframe.t -> string -> (string -> Dataframe.t -> data_object) -> data_object
    val isMemOfSeq : 'a -> 'a Seq.t -> bool
    val getUniqueValues : Dataframe.t -> string -> data_object Seq.t
    val applyOneColumnAggregate : string * (string -> Dataframe.t -> data_object) -> Dataframe.t -> data_object
    val createRow : Dataframe.t -> string -> (string * (string -> Dataframe.t -> 'a)) list -> data_object -> 'a list
    val compute_column_widths : Dataframe.t -> int list
    val print_separator : int list -> unit
    val string_of_data_object : data_object -> string
    val iloc_helper : 'a Seq.t -> int -> int -> int -> 'a Seq.t -> bool -> (bool * 'a Seq.t)
    val set_index : string -> Dataframe.t -> dataframeLoc
end

module Lib_utils : LIB_UTILS = 
struct

    let rec update_pos l new_el pos curr_pos = 

        match l with 
        | [] -> []
        | h :: t -> 
            if pos = curr_pos then new_el :: t
            else h :: update_pos t new_el pos (curr_pos + 1)

    let rec convert col_idx mapped_column original_column original_row = 
                    
        (* Sequence containing new values *)
        match mapped_column () with 
        | Seq.Nil -> Seq.Nil
        | Seq.Cons(h', t') -> 
            
            (* Sequence containing old values *)
            match original_column () with
            | Seq.Nil -> failwith "Unexpected error in Lib.map"
            | Seq.Cons(h, t) ->

                (* Current row *)
                match original_row () with
                | Seq.Nil -> failwith "Unexpected error in Lib.map"
                | Seq.Cons(rowh, rowt) ->

                    (* Change the element at index col_idx *)
                    let new_row = update_pos rowh h' col_idx 0 in

                    Seq.Cons(new_row, fun () -> convert col_idx t' t rowt)

    let rec filter_rows col_idx filtered_column original_column original_row = 

        (* Values present in the filtered column *)
        match filtered_column () with 
        | Seq.Nil -> Seq.Nil
        | Seq.Cons(h', t') ->

            (* Original sequence *)
            match original_column () with
            | Seq.Nil -> failwith "Unexpected error in Lib.map"
            | Seq.Cons(h, t) ->

                (* Current row *)
                match original_row () with
                | Seq.Nil -> failwith "Unexpected error in Lib.map"
                | Seq.Cons(rowh, rowt) ->

                    (* Take current row if values match *)
                    if h = h' then
                        Seq.Cons(rowh, fun () -> filter_rows col_idx t' t rowt)

                    else 
                        filter_rows col_idx filtered_column t rowt

    (* Removes an element at a specific index from a list.
   Throws "index out of bounds" if index is invalid.
   Uses a helper function to build the result list recursively. *)
    let removeElement_atIndex index row =
        let len = List.length row in
        if (index < 0 || index >= len) then failwith "index out of bounds"
        else
            let rec removeHelper targIndex lst currIndex ansList = 
            match lst with 
            | hd::tl ->
                if currIndex = targIndex then 
                let l_reversed = List.rev ansList in
                (l_reversed @ tl)
                else  removeHelper targIndex tl (currIndex+1) (hd::ansList)
            | [] -> []
            in
            removeHelper index row 0 []

    (* Removes a field (column) from a row based on column name.
      Gets column index using Dataframe.get_column_index and then
      calls removeElement_atIndex to perform the removal. *)
    let removeField row colName df =
        let targIndex = Dataframe.get_column_index df colName in
  
        removeElement_atIndex targIndex row

    (* Helper function that finds a value at a specific index in a row.
      Throws error for invalid indexes or empty rows.
      Recursively traverses the list until reaching target index. *)
    let rec findValueHelper targIndex row currIndex = 
        let len = List.length row in
        (* Printf.printf "len = '%d'\n" len; *)
        if (targIndex < 0 || targIndex >= len) then failwith "target Index out of bounds"
        else
            match row with
        | [] -> failwith "Can't find value in an empty row"
        | h::t -> 
            if (currIndex = targIndex) then (List.nth row currIndex)
        else                            findValueHelper targIndex row (currIndex+1)  

    (* Finds a value in a row based on column name.
      Gets column index using Dataframe.get_column_index and then
      calls findValueHelper to locate the value. *)
    let findValue colName row df =
        let targIndex = Dataframe.get_column_index df colName in
        (* Printf.printf "Looking for column '%s' at index %d\n" colName targIndex; *)
      
        findValueHelper targIndex row 0

     (* Merges two records into a single record when they have matching values in the specified column.
      Compares values of the same type and creates a new record by 
      appending first record with second record (minus the join column).
      Returns empty list if values don't match. *)
    let mergeIntoSingleRecord item1 item2 colName df1 df2 =
        let value1 = findValue colName item1 df1 in
        let value2 = findValue colName item2 df2 in
        
        match (value1, value2) with
        | (INT_DATA i1, INT_DATA i2) when i1 = i2 -> item1 @ (removeField item2 colName df2)
        | (STRING_DATA s1, STRING_DATA s2) when s1 = s2 -> item1 @ (removeField item2 colName df2)
        | (FLOAT_DATA f1, FLOAT_DATA f2) when f1 = f2 -> item1 @ (removeField item2 colName df2)
        | (BOOL_DATA b1, BOOL_DATA b2) when b1 = b2 -> item1 @ (removeField item2 colName df2)
        | (CHAR_DATA c1, CHAR_DATA c2) when c1 = c2 -> item1 @ (removeField item2 colName df2)
        | _ -> []

    (* Converts a dataframe's row sequence to a list for easier processing.
      Uses seq_to_list to perform the conversion. *)
    let seq_to_list seq =
        let rec aux acc seq =
            match seq () with
            | Seq.Nil -> List.rev acc  (* Reverse to maintain original order *)
            | Seq.Cons (x, rest) -> aux (x :: acc) rest
        in
        aux [] seq

    (* Recursively joins an item with each element in a sequence based on a common column.
      For each element in the sequence, tries to merge with the item and creates a new sequence.
      Uses Seq.append to combine results. *)
    let rec joinItemWithList item l colName df1 df2 = 

        match l () with 
        Seq.Nil -> Seq.empty
        | Seq.Cons(hd, tl) -> Seq.append (fun () -> Seq.Cons(mergeIntoSingleRecord item hd colName df1 df2, Seq.empty)) (joinItemWithList item tl colName df1 df2)

    (* Creates a new dataframe from joined rows with combined schemas from two parent dataframes.
      Handles empty result case.
      Creates combined headers by removing duplicates from the second dataframe.
      Creates matching combined datatypes.
      Returns a new dataframe with the combined schema and joined rows. *)
    let convertToDataFrame rows df1 df2  =
        
        match rows () with
        | Seq.Nil -> { 
                headers = [];
                dtypes = [];
                rows = Seq.empty;
                ncols = 0;
            }

        | _ -> 

            (* Create combined headers by removing duplicates of the join column *)
            let combined_headers = 
            let row1_headers = df1.headers in
            let row2_headers_filtered = List.filter (fun h -> not (List.mem h row1_headers)) df2.headers in
            row1_headers @ row2_headers_filtered 
            in
            
            (* Create combined dtypes (matching the combined headers) *)
            let combined_dtypes =
            let row1_dtypes = df1.dtypes in
            let row2_dtypes_filtered = 
                let rec filter_dtypes headers dtypes acc excluded_headers =
                match (headers, dtypes) with
                | ([], _) | (_, []) -> List.rev acc
                | (h::hs, d::ds) -> 
                    if List.mem h excluded_headers then
                        filter_dtypes hs ds acc excluded_headers
                    else
                        filter_dtypes hs ds (d :: acc) excluded_headers
                in
                filter_dtypes df2.headers df2.dtypes [] df1.headers
            in
            row1_dtypes @ row2_dtypes_filtered
            in
            
            (* Return the new dataframe *)
            let new_df : Dataframe.t = 
            {
                headers = combined_headers;
                dtypes = combined_dtypes;
                rows = rows;
                ncols = List.length combined_headers;
            } in

            new_df

    (* Creates a new dataframe using the parent dataframe's schema but with different rows.
      Preserves headers, datatypes, and column count from parent dataframe. *)
    let convertRowsToDataframe parent_df filtered_rows =
        (* Create a new dataframe using the parent's headers and datatypes,
            but with the filtered rows *)
        
        let df : Dataframe.t = 
        {
            headers = parent_df.headers;
            dtypes = parent_df.dtypes;
            rows = filtered_rows;
            ncols = parent_df.ncols;
        } in 

        df

    (* Creates a standard dataframe from a dataframeLoc and a sequence of rows.
      Uses the schema information from dataframeLoc. *)
    let convertRowsToDataframeLoc parent_df filtered_rows =
        {
            headers = parent_df.lheaders;
            dtypes = parent_df.ldtypes;
            rows = filtered_rows;
            ncols = parent_df.lncols;
        }

    (* Extracts an integer value from a data_object.
      Throws error if the data_object is not an INT_DATA. *)
    let extract_int entry = 
        match entry with 
        | INT_DATA i -> i
        | _ -> failwith "Expected an integer value"

    (* Extracts a float value from a data_object.
      Converts INT_DATA to float if needed.
      Throws error if data_object is neither FLOAT_DATA nor INT_DATA. *)
    let extract_float entry = 
        match entry with 
        | FLOAT_DATA i -> i
        | INT_DATA i -> float_of_int i
        | _ -> failwith "Expected a float value"

    (* Maps a sequence of data_objects to a sequence of integers.
      Uses Operations.map with extract_int function. *)
    let get_int_values seq = 
        Operations.map (fun entry -> extract_int entry) seq

    (* Maps a sequence of data_objects to a sequence of floats.
      Uses Seq.map with extract_float function. *)
    let get_float_values seq = 
        Seq.map (fun entry -> extract_float entry) seq

    (* Function to convert a data_object to a string representation *)
    let get_rows_as_list df = seq_to_list df.rows

    (* Applies an aggregation function to a specific column in a dataframe.
      Takes a column name and a function that operates on that column. *)
    let singleAggregateResult df colName f = 
        (f colName df)

    (* Checks if an element is a member of a sequence.
      Uses Operations.mem function. *)
    let isMemOfSeq element seq = Operations.mem element seq

    (* Returns a sequence of unique values in a specified column.
      Gets all column values and filters for uniqueness using a helper function.
      Builds result incrementally, checking each value against already seen values. *)
    let getUniqueValues df colName = 
        let allColValues = Dataframe.get_column df colName in
        let rec unique_helper seenTillNow remaining = 
          match Seq.uncons remaining with
          | None -> seenTillNow
          | Some(first, rest) ->
            if (isMemOfSeq first seenTillNow) then unique_helper seenTillNow rest
            else unique_helper (Seq.cons first seenTillNow) rest
        in
      
        unique_helper Seq.empty allColValues

    (* Applies an aggregation function to a column based on a column name and function mapping.
      Extracts column name and function from the mapping tuple and calls singleAggregateResult. *)
    let applyOneColumnAggregate mapping df = 
        match mapping with
        | (colName, f) -> singleAggregateResult df colName f

    (* Creates a row by filtering dataframe for rows matching a value and applying aggregation functions.
      Defines a helper function to check if a row's value matches the target value.
      Filters rows, creates a filtered dataframe, and maps aggregation functions to create output row. *)
    let createRow df colName colToFnMapping value = 
        let isRowValueEqualToValue value colName row =
            let colIndex = Dataframe.get_column_index df colName in
            if (0 < colIndex && colIndex < (List.length df.headers)) then 
                let currValue = List.nth row colIndex in
                currValue = value
            else  failwith "columnIndex out of bounds"
        in
        
        let filteredRowsByValue = Operations.filter (fun row -> isRowValueEqualToValue value colName row) df.rows in
        
        let filtered_df = convertRowsToDataframe df filteredRowsByValue in
        let outputRow = List.map (fun mapping -> applyOneColumnAggregate mapping filtered_df) colToFnMapping in
        
        outputRow

    (* Converts a data_object to a formatted string representation.
      Adds type prefix (e.g., "STRING: ", "INT: ") to the string value. *)  
    let string_of_data_object = function
    | STRING_DATA s -> "STRING: " ^ s
    | FLOAT_DATA f -> "FLOAT: " ^ string_of_float f
    | BOOL_DATA b -> "BOOL: " ^ string_of_bool b
    | CHAR_DATA c -> "CHAR: " ^ String.make 1 c
    | INT_DATA i -> "INT: " ^ string_of_int i
    | NULL -> "NULL"

    (* Function to find the maximum width needed for each column *)
    let compute_column_widths df =
        
        (* Convert sequence to list for easier processing *)
        let rows_list = get_rows_as_list df in
        
        (* For each column, find the max width needed *)
        let max_widths = List.mapi (fun col_idx header ->
            let header_width = String.length header in
            let max_data_width = List.fold_left (fun max_width row ->
            if col_idx < List.length row then
                let value_str = string_of_data_object (List.nth row col_idx) in
                max max_width (String.length value_str)
            else max_width
            ) 0 rows_list in
            max header_width max_data_width
        ) df.headers in
        
        max_widths

    (* Print a horizontal separator line *)
    let print_separator widths =
        print_string "+";
        List.iter (fun w -> print_string (String.make (w + 2) '-'); print_string "+") widths;
        print_newline ()

    (* Helper function for iloc to extract a slice of rows from a sequence.
      Takes start and end indices and returns rows in that range.
      Returns a tuple with a boolean indicating completion and the resulting sequence.
      Throws errors for negative indices or when start > end. *)
    let rec iloc_helper rows startIndex endIndex currIndex ans reachedEnd = 
        if (startIndex < 0 || endIndex < 0) then failwith "negative indices"
        else if (startIndex > endIndex) then failwith "startIndex must be less than or equal to the endIndex"
        else
            match (Seq.uncons rows) with
            | Some(row, rest) ->
            if (currIndex > endIndex)  then (true, ans)
            else if (startIndex <= currIndex && currIndex <= endIndex) then iloc_helper rest startIndex endIndex (currIndex+1) (Seq.append ans (List.to_seq [row])) reachedEnd
            else (iloc_helper rest startIndex endIndex (currIndex+1) ans reachedEnd)
            | None -> 
            (currIndex > endIndex, ans)
    
    
    let rec index_of item lst =
        let rec aux i = function
            | [] -> -1
            | h :: t -> if h = item then i else aux (i + 1) t
        in
        aux 0 lst

    (* Creates a dataframeLoc with row indices based on values in a specified column.
      Creates a hashtable mapping values in the index column to row positions.
      Handles different data types by converting them to strings.
      Used for label-based indexing in the dataframe. *)
    let set_index column_name df =
        (* Find the column index for the specified column name *)
        let col_idx = index_of column_name df.headers in
        if col_idx = -1 then
            failwith (Printf.sprintf "Column '%s' not found" column_name)
        else
            (* Create a new hashtable to store the indices *)
            let indices = Hashtbl.create 16 in
            
            (* Convert rows to list for easier indexing *)
            let rows_list = seq_to_list df.rows in
            
            (* Fill the hashtable with row labels and their indices *)
            List.iteri (fun row_idx row ->
            match List.nth_opt row col_idx with
            | Some (STRING_DATA label) -> Hashtbl.add indices label row_idx
            | Some data_obj -> 
                (* Convert other data types to string and use as label *)
                let label = string_of_data_object data_obj in
                Hashtbl.add indices label row_idx
            | None -> () (* Skip if the column doesn't exist in this row *)
            ) rows_list;
            
            (* Create a new dataframe with the indices *)
            { 
                lheaders = df.headers;
                ldtypes = df.dtypes;
                lrows = List.to_seq rows_list; (* Convert back to seq *)
                lncols = df.ncols;
                lindices = Some indices;
            } 

end
