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

  let no_of_rows df = Seq.fold_left (fun acc _ -> acc + 1) 0 df.rows

  let size df = df.ncols * no_of_rows df

  let shape df = (df.ncols, no_of_rows df)

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
        else List.map Datatype.string_to_datatype dtypes
      with End_of_file -> 
        close_in file;
        failwith "No datatypes provided for columns" in
    let rec readlines () = 
      try
        let line = input_line file in
        let data = String.split_on_char ',' line in
        let row = Row.row_from_string_list types ncols data in
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

    let load_from_json filepath = 
      let file = open_in filepath in
      let rec iter acc = 
      try
        let line = input_line file in
        iter (acc ^ line)
      with End_of_file -> 
        close_in file;
        acc in
      let json_string = iter "" in
      let json_obj = 
        try JSON.parse json_string 
        with _ -> failwith ("Failed to parse JSON") in

      let headers = 
        try JSON.get_value json_obj "columns" 
        with _ -> failwith "Missing key columns" in

      let headers = 
        try JSON_Value.array_to_list (headers)
        with _ -> failwith "columns must be an array" in

      let headers = 
        try List.map JSON_Value.string_value_to_string headers
        with _ -> failwith "Invalid datatype for column name" in

      let ncols = List.length headers in

      let dtypes = 
        try JSON.get_value json_obj "datatypes"
        with _ -> failwith "Missing key datatypes" in

      let dtypes = 
        try JSON_Value.array_to_list dtypes
        with _ -> failwith "datatypes must be an array" in

      let _ = 
        if (List.length dtypes) = ncols then ()
        else failwith "Invalid sizes of columns and datatypes" in

      let dtypes = 
        try List.map Datatype.string_to_datatype (List.map JSON_Value.string_value_to_string dtypes)
        with _ -> failwith "Invalid datatype for column name" in

      let data = 
        try JSON.get_value json_obj "data"
        with _ -> failwith "Missing key data" in

      let data = 
        match data with 
          ARRAY (a) -> a
        | _ -> failwith "Data should be provided as an array of arrays" in


      let data = 
        try List.map JSON_Value.array_to_list data
        with _ -> failwith "Data should be provided as an array of arrays" in

        
      let rows = Seq.map (Row.row_from_json_value_list dtypes ncols) (List.to_seq data) in

      {
        headers = headers;
        dtypes = dtypes;
        rows = rows;
        ncols = ncols;
      }
      
end