open Datatypes
open Data_object

module Row = struct
  type t = data_object list

  let display_row l =
    let rec iter i =
      if i >= List.length l then Printf.printf "\n"
      else 
        let _ = print_string ((DataObject.to_string (List.nth l i)) ^ " ") in
        iter (i+1) in
    iter 0

  let row_from_list l ncols dtypes = 
    let rec iter i acc =
      if i >= ncols then acc
      else
        if i >= List.length l then NULL :: (iter (i+1) acc)
        else 
            let rowi = List.nth l i in
            let casted_rowi = DataObject.from_string (List.nth dtypes i) rowi in
            casted_rowi :: (iter (i+1) acc) in
      iter 0 []
end