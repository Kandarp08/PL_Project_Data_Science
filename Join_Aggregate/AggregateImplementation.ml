type dynamic_value =
  | Int of int
  | String of string
  | Float of float
  | Bool of bool

(* Define dynamic record type *)
type dynamic_record = (string * dynamic_value) list

let df = [
  [("id", Int 1); ("product", String "Laptop"); ("category", String "Electronics"); ("price", Int 1200); ("quantity", Int 5)];
  [("id", Int 2); ("product", String "Headphones"); ("category", String "Electronics"); ("price", Int 300); ("quantity", Int 10)];
  [("id", Int 3); ("product", String "Chair"); ("category", String "Furniture"); ("price", Int 250); ("quantity", Int 8)];
  [("id", Int 4); ("product", String "Desk"); ("category", String "Furniture"); ("price", Int 600); ("quantity", Int 3)];
  [("id", Int 5); ("product", String "Monitor"); ("category", String "Electronics"); ("price", Int 400); ("quantity", Int 7)]
];;

let rec getColList df colName = (*WORKS*)
  match df with 
  [] -> []
  | df_hd::df_tl -> 
    let colListHead = List.assoc colName df_hd in
    colListHead::(getColList df_tl colName);;

(* getColList df "product";; *)

(*-------SOME SAMPLE AGGREGATE FUNCTIONS-------*)
(* Sum function for dynamic_value list *)

let extract_int = function
| Int i -> i
| _ -> failwith "Expected Int value"

(* Extract the float value or convert an Int to Float *)
let extract_float = function
| Float f -> f
| Int i -> float_of_int i
| _ -> failwith "Expected numeric value"

let get_int_values dyn_values =
  List.map extract_int dyn_values

let get_float_values dyn_values =
  List.map extract_float dyn_values

let sum_dynamic dyn_values =
  let int_values = get_int_values dyn_values in
  Int (List.fold_left (+) 0 int_values)

let average_dynamic dyn_values =
  let num_values = get_float_values dyn_values in
  let total = List.fold_left (+.) 0.0 num_values in
  let count = float_of_int (List.length num_values) in
  Float (total /. count)


let singleAggregateResult df colName f = (*WORKS*)
  let colValuesList = getColList df colName in 
  (f colValuesList);;

(* singleAggregateResult df "price" average_dynamic;; *)

(*--------IMPLEMENTING MULTIPLE AGGREGATE BELOW---------------*)

let applyOneColumnAggregate mapping df_filtered = (*WORKS*)
  match mapping with 
  | (colName, f) -> 
    (colName, (singleAggregateResult df_filtered colName f) );;


let getValueWiseAggregate df colName colToFnMapping value = (*WORKS*)
  let isRowValueSame value colName dynamicRecord =
    try
      let currValue = List.assoc colName dynamicRecord in
      currValue = value
    with Not_found -> false in

  let filteredDFByValue = List.filter (fun record -> isRowValueSame value colName record) df in
  
  (value, List.map (fun mapping -> applyOneColumnAggregate mapping filteredDFByValue) colToFnMapping);;

let getUniqueValues df colName = (*WORKS*)
    let all_values = getColList df colName in  (*USING GETCOLLIST WRITTEN EARLIER*)
    let rec unique_helper seen = function
      | [] -> seen
      | x::xs -> 
          if List.mem x seen then unique_helper seen xs
          else unique_helper (x::seen) xs
    in
    unique_helper [] all_values;;

  
let groupByAggregate df colName colToFnMapping = 
  let uniqueValues = (getUniqueValues df colName) in

  List.map (getValueWiseAggregate df colName colToFnMapping) uniqueValues;;

let returnProduct l = 
  String ("Product");;

let aggregations = [("id", sum_dynamic); ("product", returnProduct); ("price", sum_dynamic); ("quantity", average_dynamic)];;
(groupByAggregate df "category" aggregations);; 
