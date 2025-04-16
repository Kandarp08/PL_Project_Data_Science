type dynamic_value =
  | Int of int
  | String of string
  | Float of float
  | Bool of bool

(* Define dynamic record type *)
type dynamic_record = (string * dynamic_value) list

(* Create dynamic customer records manually *)
let dynamic_customers = [
  (* Alice *)
  [("id", Int 1); ("name", String "Alice"); ("city_id", Int 101)];
  
  (* Bob *)
  [("id", Int 2); ("name", String "Bob"); ("city_id", Int 102)];
  
  (* Charlie *)
  [("id", Int 3); ("name", String "Charlie"); ("city_id", Int 101)];
  
  (* Diana *)
  [("id", Int 4); ("name", String "Diana"); ("city_id", Int 105)]
]

(* Create dynamic city records manually *)
let dynamic_cities = [
  (* New York *)
  [("city_id", Int 101); ("city", String "New York"); ("country", String "USA")];
  
  (* London *)
  [("city_id", Int 102); ("city", String "London"); ("country", String "UK")];
  
  (* Paris *)
  [("city_id", Int 103); ("city", String "Paris"); ("country", String "France")];
  
  (* Tokyo *)
  [("city_id", Int 104); ("city", String "Tokyo"); ("country", String "Japan")]
]

(*-------CONVERTED STATIC RECORDS TO DYNAMIC RECORDS----------*)

let removeField dRecord colName =     (*WORKS*)
  List.remove_assoc colName dRecord

let mergeIntoSingleRecord item1 item2 colName =
  let value1 = List.assoc colName item1 in
  let value2 = List.assoc colName item2 in
  
  match (value1, value2) with
  | (Int i1, Int i2) when i1 = i2 -> item1 @ (removeField item2 colName)
  | (String s1, String s2) when s1 = s2 -> item1 @ (removeField item2 colName)
  | (Float f1, Float f2) when f1 = f2 -> item1 @ (removeField item2 colName)
  | (Bool b1, Bool b2) when b1 = b2 -> item1 @ (removeField item2 colName)
  | _ -> []

(* mergeIntoSingleRecord item item1 colName;; *)

let rec joinItemWithList item l colName = 
  match l with 
  [] -> []
  | l_hd :: l_tl -> (mergeIntoSingleRecord item l_hd colName) :: (joinItemWithList item l_tl colName);;

let rec join l3 l4 colName = (*WORKS*)
  match (l3, l4) with
  | ([], _) -> []
  | (_, []) -> []
  | (l3_hd::l3_tl, l4_hd::l4_tl) -> 
    let list_including_emptyLists = (joinItemWithList l3_hd l4 colName) @ (join l3_tl l4 colName) in
    List.filter (fun subList -> subList <> []) list_including_emptyLists;;

join dynamic_customers dynamic_cities "city_id";;

