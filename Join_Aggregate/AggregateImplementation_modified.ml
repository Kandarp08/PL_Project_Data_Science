type datatype = 
  INT
| FLOAT
| STRING
| BOOL
| CHAR

type data_object = 
  STRING_DATA of string 
| FLOAT_DATA of float
| BOOL_DATA of bool
| CHAR_DATA of char
| INT_DATA of int
| NULL;;

module Row = struct
  type t = data_object list
end

type dataframe = {
  headers: string list;
  dtypes: datatype list;
  rows: Row.t Seq.t;
  ncols: int;
};;

let df1 = {
  headers = ["id"; "name"; "city_id"];
  dtypes = [INT; STRING; STRING];
  rows = List.to_seq [
    [INT_DATA 1; STRING_DATA "Alice"; INT_DATA 101;];
    [INT_DATA 2; STRING_DATA "Bob"; INT_DATA 102;];
    [INT_DATA 3; STRING_DATA "Charlie"; INT_DATA 101;];
    [INT_DATA 4; STRING_DATA "Diana"; INT_DATA 105;];
  ];
  ncols = 3;
};;

let convertRowsToDataframe parent_df filtered_rows =
  (* Create a new dataframe using the parent's headers and datatypes,
      but with the filtered rows *)
  {
    headers = parent_df.headers;
    dtypes = parent_df.dtypes;
    rows = filtered_rows;
    ncols = parent_df.ncols;
  }

(*1. MODIFYING THE SINGLE AGGREGATE FUNCTION FIRST*)

let get_column_index df colname = 
  let rec iter i =
    if i >= df.ncols then failwith "Column not found"
    else 
      if List.nth df.headers i = colname then i
      else iter (i+1) in
  iter 0

let getColSeq df colname = 
  let col_index = get_column_index df colname in
  Seq.map (fun row -> List.nth row col_index) df.rows 


let string_of_data_object = function
| STRING_DATA s -> "STRING: " ^ s
| FLOAT_DATA f -> "FLOAT: " ^ string_of_float f
| BOOL_DATA b -> "BOOL: " ^ string_of_bool b
| CHAR_DATA c -> "CHAR: " ^ String.make 1 c
| INT_DATA i -> "INT: " ^ string_of_int i
| NULL -> "NULL";;

(* Function to print each element in a sequence of data_objects *)
let print_data_object_seq seq =
  Seq.iter (fun data_obj -> 
    print_endline (string_of_data_object data_obj)
  ) seq;;

let extract_int entry = 
  match entry with 
  | INT_DATA i -> i
  | _ -> failwith "Expected an integer value";;

let extract_float entry = 
  match entry with 
  | FLOAT_DATA i -> i
  | INT_DATA i -> float_of_int i
  | _ -> failwith "Expected a float value";;

let get_int_values seq = 
  Seq.map (fun entry -> extract_int entry) seq;;

let get_float_values seq = 
    Seq.map (fun entry -> extract_float entry) seq;;

let sumOfSeq seq = 
  let intSeq = get_int_values seq in
  INT_DATA (Seq.fold_left (+) 0 intSeq);;

let lengthOfSeq seq =
  Seq.fold_left (fun count _ -> count + 1) 0 seq

let averageOfSeq seq = 
  let floatSeq = get_float_values seq in
  let totalSum = Seq.fold_left (+.) 0.0 floatSeq in
  let count = lengthOfSeq floatSeq in
  FLOAT_DATA (totalSum /. (float_of_int count))

let countNoOfElements seq =
  INT_DATA (Seq.fold_left (fun count _ -> count + 1) 0 seq);;


(*-----FUNCTIONS TO VISUALISE A DATAFRAME------*)
let seq_to_list (seq : 'a Seq.t) : 'a list =
  let rec aux acc seq =
    match seq () with
    | Seq.Nil -> List.rev acc  (* Reverse to maintain original order *)
    | Seq.Cons (x, rest) -> aux (x :: acc) rest
  in
  aux [] seq;;

(* Function to convert a data_object to a string representation *)
let get_rows_as_list (df : dataframe) : Row.t list =
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

(*--------------END: VISUALISING FUNCTION CODE----------*)

(*------------------START: TESTING getColSeq-----------------*)

(* let nameColSeq = getColSeq df1 "name";; WORKS *)
(* let city_id_ColSeq = getColSeq df1 "city_id";; WORKS *)

(*---BELOW CODE IS FOR VISUALISATION OF THE nameColSeq------*)

(* print_endline "Contents of nameColSeq:";
print_data_object_seq nameColSeq;;

print_endline "Contents of city_id_Seq:";
print_data_object_seq city_id_ColSeq;; WORKS *)

(*--END: VISUALISATION OF name/city_idColSeq----*)

(*----------------END: TESTING getColSeq--------------------------*)
let singleAggregateResult df colName f = (*WORKS*)
  let colValuesSeq = getColSeq df colName in 
  (f colValuesSeq)

(* singleAggregateResult df1 "city_id" sumOfSeq;; WORKS *)
(* singleAggregateResult df1 "id" averageOfSeq;; WORKS *)
  

(*2. MODIFYING THE GROUPBY FUNCTION AFTER THAT*)
let isMemOfSeq element seq =
  let rec aux s =
    match Seq.uncons s with
    | None -> false
    | Some (x, xs) -> 
        if x = element then true
        else aux xs
  in
  aux seq;;

let getUniqueValues df colName = 
  let allColValues = getColSeq df colName in
  let rec unique_helper seenTillNow remaining = 
    match Seq.uncons remaining with
    | None -> seenTillNow
    | Some(first, rest) ->
      if (isMemOfSeq first seenTillNow) then unique_helper seenTillNow rest
      else unique_helper (Seq.cons first seenTillNow) rest
  in

  unique_helper Seq.empty allColValues ;;

(* let cityIdUniqueValues = getUniqueValues df1 "city_id";;
print_data_object_seq cityIdUniqueValues;; WORKS *)

let applyOneColumnAggregate mapping df = 
  match mapping with
  | (colName, f) -> singleAggregateResult df colName f;;


let createRow df colName colToFnMapping value = (*WORKS*)
  let isRowValueEqualToValue value colName row =
      let colIndex = get_column_index df colName in
      if (0 < colIndex && colIndex < (List.length df.headers)) then 
        let currValue = List.nth row colIndex in
        currValue = value
      else  failwith "columnIndex out of bounds"
  in
  
  let filteredRowsByValue = Seq.filter (fun row -> isRowValueEqualToValue value colName row) df.rows in
  
  (*A ROW IS A LIST OF DATA_OBJECTS, THEREFORE USE List.map TO CREATE THE ROW*)
  let filtered_df = convertRowsToDataframe df filteredRowsByValue in
  let outputRow = List.map (fun mapping -> applyOneColumnAggregate mapping filtered_df) colToFnMapping in
  outputRow;;

(* let colMapping1 = [("id", sumOfSeq); ("name", fun seq -> STRING_DATA "name"); ("city_id", sumOfSeq)];; *)
let colMapping1 = [("id", sumOfSeq); ("name", countNoOfElements); ("city_id", sumOfSeq)];;

let groupByAggregate df colName colToFnMapping = 
  let uniqueValues = (getUniqueValues df colName) in

  let groupedRows = Seq.map (createRow df colName colToFnMapping) uniqueValues in
  convertRowsToDataframe df groupedRows;;

let groupedRows1 = groupByAggregate df1 "city_id" colMapping1 in
display_dataframe groupedRows1;;









(*-----------------TESTING CODE FOR createRow (WORKS)---------------*)
(* let colMapping1 = [("id", sumOfSeq); ("name", fun seq -> STRING_DATA "name"); ("city_id", averageOfSeq)] in
let row1 = createRow df1 "city_id" colMapping1 (INT_DATA 107) in

print_data_object_seq (List.to_seq row1);; *)
(*-----------------------------END: TESTING CODE ---------------------*)

(*----START: TESTING FILTERING PART (WORKS) -----*)

(* let isRowValueEqualToValue value colName row =
  let colIndex = get_column_index df1 colName in
  if (0 < colIndex && colIndex < (List.length df1.headers)) then 
    let currValue = List.nth row colIndex in
    if currValue = value then true
    else                      false
  else  failwith "columnIndex out of bounds";;

let filteredDFByValue = Seq.filter (fun row -> isRowValueEqualToValue (INT_DATA 103) "city_id" row) df1.rows in
let filtered_df = convertRowsToDataframe df1 filteredDFByValue in
display_dataframe filtered_df;;  *)

(*-----END: TESTING FILTERING PART------*)

