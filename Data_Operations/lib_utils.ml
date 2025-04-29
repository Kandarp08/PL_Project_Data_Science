open Datatypes
open Dataframe
open Data_object
open Row

open Operations
open Int_util
open Float_util

module type LIB_UTILS = 
sig
    val convert : int -> data_object Seq.t -> data_object Seq.t -> Row.t Seq.t -> Row.t Seq.node
    val filter_rows : int -> data_object Seq.t -> data_object Seq.t -> Row.t Seq.t -> Row.t Seq.node
    val removeElement_atIndex : int -> 'a list -> 'a list
    val removeField : data_object list -> string -> Dataframe.t -> data_object list
    val findValueHelper : int -> 'a list -> int -> 'a
    val findValue : string -> 'a list -> Dataframe.t -> 'a
    val mergeIntoSingleRecord : data_object list -> data_object list -> string -> Dataframe.t -> Dataframe.t -> data_object list
    val get_rows_as_list : Dataframe.t -> Row.t list
    val joinItemWithList : data_object list -> data_object list list -> string -> Dataframe.t -> Dataframe.t -> data_object list list
    val convertToDataFrame : Row.t list -> Dataframe.t -> Dataframe.t -> Dataframe.t
    val convertRowsToDataframe : Dataframe.t -> Row.t Seq.t -> Dataframe.t
    val string_of_data_object : data_object -> string
    val extract_int : data_object -> int
    val extract_float : data_object -> float
    val get_int_values : data_object Seq.t -> int Seq.t
    val get_float_values : data_object Seq.t -> float Seq.t
    val seq_to_list : 'a Seq.t -> 'a list
    val get_rows_as_list : Dataframe.t -> Row.t list
    val singleAggregateResult : Dataframe.t -> string -> (string -> Dataframe.t -> data_object) -> data_object
    val isMemOfSeq : 'a -> 'a Seq.t -> bool
    val getUniqueValues : Dataframe.t -> string -> data_object Seq.t
    val applyOneColumnAggregate : string * (string -> Dataframe.t -> data_object) -> Dataframe.t -> data_object
    val createRow : Dataframe.t -> string -> (string * (string -> Dataframe.t -> 'a)) list -> data_object -> 'a list
    val compute_column_widths : Dataframe.t -> int list
    val print_separator : int list -> unit
    val string_of_data_object : data_object -> string
end

module Lib_utils : LIB_UTILS = 
struct

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
                    let old_row = Array.of_list rowh in
                    old_row.(col_idx) <- h';
                    let new_row = (Array.to_list old_row) in

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

    let removeField row colName df =
        let targIndex = Dataframe.get_column_index df colName in
  
        removeElement_atIndex targIndex row

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
        
    let findValue colName row df =
        let targIndex = Dataframe.get_column_index df colName in
        (* Printf.printf "Looking for column '%s' at index %d\n" colName targIndex; *)
      
        findValueHelper targIndex row 0

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

    let seq_to_list seq =
        let rec aux acc seq =
            match seq () with
            | Seq.Nil -> List.rev acc  (* Reverse to maintain original order *)
            | Seq.Cons (x, rest) -> aux (x :: acc) rest
        in
        aux [] seq
        
    let rec joinItemWithList item l colName df1 df2 = 
        match l with 
        [] -> []
        | hd :: tl -> (mergeIntoSingleRecord item hd colName df1 df2) :: (joinItemWithList item tl colName df1 df2);;

    let convertToDataFrame rows df1 df2  =
        (* If no rows, return an empty dataframe with combined headers *)
        if rows = [] then 
            { 
            headers = [];
            dtypes = [];
            rows = Seq.empty;
            ncols = 0;
            }
        else
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
            {
            headers = combined_headers;
            dtypes = combined_dtypes;
            rows = List.to_seq rows;
            ncols = List.length combined_headers;
            }

    let convertRowsToDataframe parent_df filtered_rows =
        (* Create a new dataframe using the parent's headers and datatypes,
            but with the filtered rows *)
        
        {
            headers = parent_df.headers;
            dtypes = parent_df.dtypes;
            rows = filtered_rows;
            ncols = parent_df.ncols;
        }

    let string_of_data_object = function
        | STRING_DATA s -> "STRING: " ^ s
        | FLOAT_DATA f -> "FLOAT: " ^ string_of_float f
        | BOOL_DATA b -> "BOOL: " ^ string_of_bool b
        | CHAR_DATA c -> "CHAR: " ^ String.make 1 c
        | INT_DATA i -> "INT: " ^ string_of_int i
        | NULL -> "NULL"

    let extract_int entry = 
        match entry with 
        | INT_DATA i -> i
        | _ -> failwith "Expected an integer value"

    let extract_float entry = 
        match entry with 
        | FLOAT_DATA i -> i
        | INT_DATA i -> float_of_int i
        | _ -> failwith "Expected a float value"

    let get_int_values seq = 
        Operations.map (fun entry -> extract_int entry) seq

    let get_float_values seq = 
        Seq.map (fun entry -> extract_float entry) seq

    (* Function to convert a data_object to a string representation *)
    let get_rows_as_list df = seq_to_list df.rows

    let singleAggregateResult df colName f = 
        (f colName df)

    let isMemOfSeq element seq = Operations.mem element seq
    
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

        
    let applyOneColumnAggregate mapping df = 
        match mapping with
        | (colName, f) -> singleAggregateResult df colName f

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

    let seq_to_list (seq : 'a Seq.t) : 'a list =
        let rec aux acc seq =
            match seq () with
            | Seq.Nil -> List.rev acc  (* Reverse to maintain original order *)
            | Seq.Cons (x, rest) -> aux (x :: acc) rest
        in
        aux [] seq

    (* Function to convert a data_object to a string representation *)
    let get_rows_as_list df = seq_to_list df.rows

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

    let string_of_data_object = function
        | STRING_DATA s -> "STRING: " ^ s
        | FLOAT_DATA f -> "FLOAT: " ^ string_of_float f
        | BOOL_DATA b -> "BOOL: " ^ string_of_bool b
        | CHAR_DATA c -> "CHAR: " ^ String.make 1 c
        | INT_DATA i -> "INT: " ^ string_of_int i
        | NULL -> "NULL"
end