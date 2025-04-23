open Datatypes
open Data_object
open Row
open Utils

type dataframe = {
  headers: string list;
  dtypes: datatype list;
  rows: Row.t Seq.t;
  ncols: int;
}

module Dataframe = struct
  type t = dataframe

  let get_column_index df colname = 
    let rec iter i =
      if i >= df.ncols then failwith "Column not found"
      else 
        if List.nth df.headers i = colname then i
        else iter (i+1) in
    iter 0

  let get_column df colname = 
    let col_index = get_column_index df colname in
    Seq.map (fun row -> List.nth row col_index) df.rows 

  let load_from_file sep filepath = 
    let file = open_in filepath in
    let headers = 
      try
        let line1 = input_line file in
        List.map strip (String.split_on_char ',' line1)
      with End_of_file -> 
        close_in file;
        failwith "No column names" in
    let ncols = List.length headers in
    let types = 
      try
        let line2 = input_line file in
        let dtypes = String.split_on_char ',' line2 in
        if (List.length dtypes) != ncols then failwith "Datatypes were not provided for some columns"
        else List.map string_to_datatype dtypes
      with End_of_file -> 
        close_in file;
        failwith "No datatypes provided for columns" in
    let rec readlines () = 
      try
        let line = input_line file in
        let data = String.split_on_char ',' line in
        let row = Row.row_from_list data ncols types in
        Seq.Cons (row, readlines);
      with End_of_file -> 
        close_in file;
        Seq.Nil in
    {
      headers=headers;
      dtypes = types;
      rows = Seq.memoize readlines;
      ncols = ncols;
    }

    let load_from_csv = load_from_file ","
    let load_from_tsv = load_from_file "\t"
end