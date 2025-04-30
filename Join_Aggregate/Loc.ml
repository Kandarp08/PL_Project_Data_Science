(*--------------LOC FUNCTION STARTS BELOW---------------*)

type dataframeLoc = {
  headers: string list;
  dtypes: datatype list;
  rows: Row.t Seq.t;
  ncols: int;
  indices: (string, int) Hashtbl.t option; (* Maps row labels to row indices *)
}

let df2 = {
  headers = ["id"; "name"; "city_id"];
  dtypes = [INT; STRING; STRING];
  rows = List.to_seq [
    [INT_DATA 1; STRING_DATA "Alice"; INT_DATA 101;];
    [INT_DATA 2; STRING_DATA "Bob"; INT_DATA 102;];
    [INT_DATA 3; STRING_DATA "Charlie"; INT_DATA 101;];
    [INT_DATA 4; STRING_DATA "Diana"; INT_DATA 105;];
  ];
  ncols = 3;
  indices = None;  (* Initially no indices *)
};;

let convertRowsToDataframeLoc parent_df filtered_rows =
  {
    headers = parent_df.headers;
    dtypes = parent_df.dtypes;
    rows = filtered_rows;
    ncols = parent_df.ncols;
    indices = parent_df.indices;  (* Preserve existing indices *)
  }

(*-----FUNCTIONS TO VISUALISE A DATAFRAME------*)
let string_of_data_object = function
| STRING_DATA s -> "STRING: " ^ s
| FLOAT_DATA f -> "FLOAT: " ^ string_of_float f
| BOOL_DATA b -> "BOOL: " ^ string_of_bool b
| CHAR_DATA c -> "CHAR: " ^ String.make 1 c
| INT_DATA i -> "INT: " ^ string_of_int i
| NULL -> "NULL";;

let seq_to_list (seq : 'a Seq.t) : 'a list =
  let rec aux acc seq =
    match seq () with
    | Seq.Nil -> List.rev acc  (* Reverse to maintain original order *)
    | Seq.Cons (x, rest) -> aux (x :: acc) rest
  in
  aux [] seq;;

(* Function to convert a data_object to a string representation *)
let get_rows_as_list (df : dataframeLoc) : Row.t list =
  seq_to_list df.rows;;

(* Function to find the maximum width needed for each column *)
let compute_column_widths df =
  let header_widths = List.map String.length df.headers in
  
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

(* Display the dataframe in a tabular format *)
let display_dataframe df =
  (* If the dataframe is empty, show a message *)
  if df.headers = [] then
    print_endline "Empty dataframe"
  else
    let col_widths = compute_column_widths df in
    
    (* Print the header row *)
    print_separator col_widths;
    print_string "| ";
    List.iter2 (fun header width ->
      Printf.printf "%-*s | " width header
    ) df.headers col_widths;
    print_newline ();
    print_separator col_widths;
    
    (* Print each data row *)
    let rows_list = get_rows_as_list df in
    List.iter (fun row ->
      print_string "| ";
      List.iteri (fun i cell ->
        if i < List.length col_widths then
          let width = List.nth col_widths i in
          Printf.printf "%-*s | " width (string_of_data_object cell)
      ) row;
      print_newline ()
    ) rows_list;
    print_separator col_widths;
    
    (* Print dataframe summary *)
    Printf.printf "Dataframe with %d columns and %d rows\n" 
      df.ncols (List.length rows_list);;

(* --------------END: VISUALISING FUNCTION CODE---------- *)

let rec index_of item lst =
  let rec aux i = function
    | [] -> -1
    | h :: t -> if h = item then i else aux (i + 1) t
  in
  aux 0 lst

let set_index df column_name =
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
    { df with 
      indices = Some indices;
      rows = List.to_seq rows_list; (* Convert back to seq *)
    }


(*--iloc functions reused with slight modifications--*)

let rec iloc_helper rows targIndex currIndex = 
  if (targIndex < 0) then failwith "negative index"
  else
    match (Seq.uncons rows) with
    | Some(first, rest) ->
      if (currIndex = targIndex) then first
      else (iloc_helper rest targIndex (currIndex+1))
    | None -> 
      failwith "Index out of bounds";;

let ilocSingleRow df index = 
  let rows = df.rows in

  let outputRow = iloc_helper rows index 0 in
  convertRowsToDataframeLoc df (List.to_seq [outputRow]);;     (*outputRow is a singleElement. To convert it to a sequence, List.to_seq [outputRow] is a handy workaround*)

(*let indexedRowDF = ilocSingleRow df1 0 in
display_dataframe indexedRowDF;; WORKS*)

let rec iloc2_helper rows startIndex endIndex currIndex ans reachedEnd = 
  if (startIndex < 0 || endIndex < 0) then failwith "negative indices"
  else if (startIndex >= endIndex) then failwith "startIndex must be less than the endIndex"
  else
    match (Seq.uncons rows) with
    | Some(row, rest) ->
      if (currIndex > endIndex)  then (true, ans)
      else if (startIndex <= currIndex && currIndex <= endIndex) then iloc2_helper rest startIndex endIndex (currIndex+1) (Seq.append ans (List.to_seq [row])) reachedEnd
      else (iloc2_helper rest startIndex endIndex (currIndex+1) ans reachedEnd)
    | None -> 
      (currIndex > endIndex, ans);;

let ilocMultipleRows df startIndex endIndex = 
  if startIndex < 0 then failwith "startIndex cannot be negative"
  else if endIndex < 0 then failwith "endIndex cannot be negative"  
  else if startIndex > endIndex then failwith "startIndex must be less than or equal to endIndex"
  else
    let (reachedEnd, outputRows) = iloc2_helper df.rows startIndex endIndex 0 Seq.empty false in
    if not reachedEnd then
      failwith (Printf.sprintf "endIndex %d is out of bounds for sequence" endIndex)
    else
      convertRowsToDataframeLoc df outputRows;;

(*--END OF USING iloc functions*)


let locSingleRow df label = 
  match df.indices with
  | None -> failwith "Dataframe has no index set"
  | Some indices ->
      try
        let row_idx = Hashtbl.find indices label in
        ilocSingleRow df row_idx  (* REUSING MY EXISTING FUNCTION *)
      with Not_found ->
        failwith (Printf.sprintf "Label '%s' not found in index" label);;

let locMultipleRows df startLabel endLabel = 
  match df.indices with
  | None -> failwith "Dataframe has no index set"
  | Some indices ->
      try
        let start_idx = Hashtbl.find indices startLabel in
        let end_idx = Hashtbl.find indices endLabel in
        ilocMultipleRows df start_idx end_idx  (* REUSING MY EXISTING FUNCTION *)
      with Not_found ->
        failwith "One or more labels not found in index";;


(*---TESTING ABOVE CODE (START)----*)
let indexed_df = set_index df2 "name";;

(* Get a row by label *)
let alice_row = locSingleRow indexed_df "Alice";;
display_dataframe alice_row;;

(* Get multiple rows by label range *)
let bob_to_diana = locMultipleRows indexed_df "Bob" "Charlie";;
display_dataframe bob_to_diana;
(*--END: TESTING CODE -----*)