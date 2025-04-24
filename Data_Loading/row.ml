open Datatypes
open Data_object
open Utils

module Row = struct
  type t = data_object list

  let display_row l =
    let rec iter i =
      if i >= List.length l then Printf.printf "\n"
      else 
        let _ = print_string ((DataObject.to_string (List.nth l i)) ^ " ") in
        iter (i+1) in
    iter 0

    let row_from_list f dtypes ncols l = 
      let rec iter i acc =
        if i >= ncols then acc
        else
          if i >= List.length l then NULL :: (iter (i+1) acc)
          else 
              let rowi = List.nth l i in
              let casted_rowi = f (List.nth dtypes i) rowi in
              casted_rowi :: (iter (i+1) acc) in
        iter 0 []

  let row_from_string_list = row_from_list DataObject.from_string

  let row_from_json_value_list = row_from_list DataObject.from_json_value
end