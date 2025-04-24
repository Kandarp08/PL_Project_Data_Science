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

let rec findIndexHelper colName colNameList ans = 
  match colNameList with
  | [] -> -1
  | hd::tl -> 
    if (hd = colName) then (ans+1)
    else                   findIndexHelper colName tl (ans+1);;

let findIndex colName df =
  let colNameList = df.headers in
  findIndexHelper colName colNameList (-1);;

(* TESTING findIndex FUNCTION*)
let df2 = {
  headers = ["city_id"; "city"; "country";];
  dtypes = [INT; STRING; STRING;];
  rows =  List.to_seq [
    [INT_DATA 101; STRING_DATA "Paris"; STRING_DATA "France"];
    [INT_DATA 102; STRING_DATA "London"; STRING_DATA "UK"];
    [INT_DATA 103; STRING_DATA "Tokyo"; STRING_DATA "Japan"];
    [INT_DATA 104; STRING_DATA "New York"; STRING_DATA "USA"];
    [INT_DATA 105; STRING_DATA "Berlin"; STRING_DATA "Germany"];
  ];
  ncols = 3;
};;

(* findIndexHelper "country" ["city_id"; "city"; "country";] (-1);; WORKS*)

(* findIndex "countrys" df2;; WORKS*)

(*-----------1. TESTING findIndex FUNCTION ENDS------------*)


(*----------2. removeElement_atIndex and the modified removeField function IMPLEMENTATION AND TESTING---------*)

let removeElement_atIndex (index: int) (row: 'a list) =
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
    removeHelper index row 0 [];;

  

(* removeElement_atIndex 2 [INT_DATA 5; STRING_DATA "Berlin"; STRING_DATA "Germany"];; WORKS *)

let removeField (row: data_object list) (colName: string) df =
  let colNameList = df.headers in
  let targIndex = findIndexHelper colName colNameList (-1) in
  removeElement_atIndex targIndex row;;

(* removeField [INT_DATA 5; STRING_DATA "Berlin"; STRING_DATA "Germany"] "country" df2;; WORKS *)

(*----------END 2. removeElement_atIndex and the modified removeField function IMPLEMENTATION AND TESTING---------*)

let rec findValueHelper targIndex (row: 'a list) currIndex = 
  let len = List.length row in
  (* Printf.printf "len = '%d'\n" len; *)
  if (targIndex < 0 || targIndex >= len) then failwith "target Index out of bounds"
  else
    match row with
  | [] -> failwith "Can't find value in an empty row"
  | h::t -> 
    if (currIndex = targIndex) then (List.nth row currIndex)
    else                            findValueHelper targIndex row (currIndex+1);;  


let findValue colName (row: 'a list) df =
  let colNameList = df.headers in
  let targIndex = findIndexHelper colName colNameList (-1) in
  (* Printf.printf "Looking for column '%s' at index %d\n" colName targIndex; *)

  findValueHelper targIndex row 0;;

(* findValue "dih" [INT_DATA 5; STRING_DATA "Berlin"; STRING_DATA "Germany"] df2;; WORKS *)

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

(*START---------TESTING mergeIntoSingleRecord--------------*)
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

(* mergeIntoSingleRecord [INT_DATA 4; STRING_DATA "Diana"; INT_DATA 105;]  [INT_DATA 105; STRING_DATA "Berlin"; STRING_DATA "Germany"] "city_id" df1 df2;; *)

(*END-----------TESTING mergeIntoSingleRecord=-------------*)
  
(* Convert a sequence to a list *)
let seq_to_list (seq : 'a Seq.t) : 'a list =
  let rec aux acc seq =
    match seq () with
    | Seq.Nil -> List.rev acc  (* Reverse to maintain original order *)
    | Seq.Cons (x, rest) -> aux (x :: acc) rest
  in
  aux [] seq;;

(* Get all rows from a dataframe as a list *)
let get_rows_as_list (df : dataframe) : Row.t list =
  seq_to_list df.rows;;

let rec joinItemWithList item l colName df1 df2 = 
  match l with 
  [] -> []
  | hd :: tl -> (mergeIntoSingleRecord item hd colName df1 df2) :: (joinItemWithList item tl colName df1 df2);;


(*-------------CONVERT TO DATAFRAME OBJECT TYPE-----------*)
let convertToDataFrame (rows: Row.t list) : dataframe =
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
(*--------END: convertToDataFrame function-----------*)

let rec join df1 df2 colName =
  let l1 = get_rows_as_list df1 in
  let l2 = get_rows_as_list df2 in

  let rec joinHelper (l1: 'a list) (l2: 'a list) colName = 
    match (l1, l2) with
    | ([], _) -> []
    | (_, []) -> []
    | (l1_hd::l1_tl, l2_hd::l2_tl) -> 
      let list_including_emptyLists = (joinItemWithList l1_hd l2 colName df1 df2) @ (joinHelper l1_tl l2 colName) in
      List.filter (fun subList -> subList <> []) list_including_emptyLists
  in
  let finalJoinedList = joinHelper l1 l2 colName in
  convertToDataFrame finalJoinedList;;

let joinedDF = join df1 df2 "city_id";;

(*-----FUNCTIONS TO VISUALISE A DATAFRAME------*)
(* Function to convert a data_object to a string representation *)
let string_of_data_object = function
  | INT_DATA i -> string_of_int i
  | FLOAT_DATA f -> string_of_float f
  | STRING_DATA s -> s
  | BOOL_DATA b -> string_of_bool b
  | CHAR_DATA c -> String.make 1 c
  | NULL -> "NULL"

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
      df.ncols (List.length rows_list)

(*--------------END: VISUALISING FUNCION CODE----------*)

let () = 
  let joined_df = join df1 df2 "city_id" in
  display_dataframe joined_df
