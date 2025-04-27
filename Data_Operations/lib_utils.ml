open Datatypes
open Dataframe
open Data_object
open Row

module type LIB_UTILS = 
sig
    val convert : int -> data_object Seq.t -> data_object Seq.t -> Row.t Seq.t -> Row.t Seq.node
    val filter_rows : int -> data_object Seq.t -> data_object Seq.t -> Row.t Seq.t -> Row.t Seq.node
    val removeElement_atIndex : int -> 'a list -> 'a list
    val removeField : data_object list -> string -> Dataframe.t -> data_object list
    val findValueHelper : int -> 'a list -> int -> 'a
    val findValue : string -> 'a list -> Dataframe.t -> 'a
    val mergeIntoSingleRecord : data_object list -> data_object list -> string -> Dataframe.t -> Dataframe.t -> data_object list
    val seq_to_list : 'a Seq.t -> 'a list
    val get_rows_as_list : Dataframe.t -> Row.t list
    val joinItemWithList : data_object list -> data_object list list -> string -> Dataframe.t -> Dataframe.t -> data_object list list
    val convertToDataFrame : Row.t list -> Dataframe.t -> Dataframe.t -> Dataframe.t
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

(* Get all rows from a dataframe as a list *)
    let get_rows_as_list df = seq_to_list df.rows
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

end