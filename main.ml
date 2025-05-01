open Dataframe
open Row
open Lib
open Data_object
open Data_object.DataObject

(* Simple assertion helpers *)
let assert_equal expected actual message =
    if expected <> actual then
      (Printf.printf "FAIL: %s - Expected %s but got %s\n" message (string_of_int expected) (string_of_int actual);
       false)
    else
      (Printf.printf "PASS: %s\n" message;
       true)
  
let assert_equal_float expected actual epsilon message =
if abs_float (expected -. actual) > epsilon then
    (Printf.printf "FAIL: %s - Expected %f but got %f\n" message expected actual;
    false)
else
    (Printf.printf "PASS: %s\n" message;
    true)

let assert_true condition message =
if not condition then
    (Printf.printf "FAIL: %s\n" message;
    false)
else
    (Printf.printf "PASS: %s\n" message;
    true)

(* Helper function to generate a test dataframe *)
let create_test_df () =
    let csv_path = "test_data.csv" in
    let csv_content = "Name,Age,Salary,Department,Active\nString,Int,Float,String,Bool\nJohn,30,75000.5,Engineering,true\nAlice,25,65000.0,Marketing,true\nBob,35,,HR,false\nEve,28,72000.0,Engineering,true\n,,,,\n" in
    let oc = open_out csv_path in
    output_string oc csv_content;
    close_out oc;
    Dataframe.load_from_csv csv_path


let create_test_df2 () =
    let csv_path = "test_data2.csv" in
    let csv_content = "Name,Age,Email,Department,Score\nstring,integer,string,string,float\nJohn Doe,32,john.doe@example.com,Marketing,85.5\nJane Smith,28,jane.smith@example.com,Engineering,92.3\nRobert Johnson,41,robert.j@example.com,Finance,78.9\nEmily Wilson,35,e.wilson@example.com,HR,88.7\nMichael Brown,29,michael.b@example.com,Engineering,81.2\nSarah Davis,37,sarah.davis@example.com,Marketing,94.1\nThomas Lee,45,t.lee@example.com,Finance,76.8\nJennifer Garcia,31,j.garcia@example.com,HR,89.5\nDavid Miller,38,d.miller@example.com,IT,83.6\nLisa Wilson,26,lisa.w@example.com,Marketing,90.4" in
    let oc = open_out csv_path in
    output_string oc csv_content;
    close_out oc;
    Dataframe.load_from_csv csv_path


(* Helper function to generate a test JSON dataframe *)
let create_test_json_df () =
    let json_path = "test_data.json" in
    let json_content = {|
      {
        "columns": ["Name", "Age", "Salary", "Department", "Active"],
        "datatypes": ["String", "Int", "Float", "String", "Bool"],
        "data": [
          ["John", 30, 75000.5, "Engineering", true],
          ["Alice", 25, 65000.0, "Marketing", true],
          ["Bob", 35, null, "HR", false],
          ["Eve", 28, 72000.0, "Engineering", true],
          [null, null, null, null, null]
        ]
      }
    |} in
    let oc = open_out json_path in
    output_string oc json_content;
    close_out oc;
    Dataframe.load_from_json json_path

(* Helper function to create a second dataframe for join testing *)
let create_join_df () =
    let csv_path = "join_data.csv" in
    let csv_content = "Department,Location,EmployeeCount\nString,String,Int\nEngineering,New York,50\nMarketing,San Francisco,30\nHR,Chicago,15\nFinance,Boston,20\n" in
    let oc = open_out csv_path in
    output_string oc csv_content;
    close_out oc;
    Dataframe.load_from_csv csv_path

(* Run a test and print the result *)
let run_test name test_func =
    Printf.printf "Running test: %s\n" name;
    try
      let result = test_func () in
      if result then
        Printf.printf "Test %s: PASSED\n\n" name
      else
        Printf.printf "Test %s: FAILED\n\n" name
    with e ->
      Printf.printf "Test %s: EXCEPTION - %s\n\n" name (Printexc.to_string e)


let test_load_from_csv () =
    let df = create_test_df () in
    let rows, cols = Dataframe.shape df in
    assert_equal 5 rows "Dataframe should have 5 rows" &&
    assert_equal 5 cols "Dataframe should have 5 columns"

let test_load_from_json () =
    let df = create_test_json_df () in
    let rows, cols = Dataframe.shape df in
    assert_equal 5 rows "Dataframe should have 5 rows" &&
    assert_equal 5 cols "Dataframe should have 5 columns"

let test_no_of_rows () =
    let df = create_test_df () in
    assert_equal 5 (Dataframe.no_of_rows df) "Dataframe should have 5 rows"

let test_size () =
    let df = create_test_df () in
    assert_equal 25 (Dataframe.size df) "Dataframe size should be 25 (5 rows * 5 columns)"

let test_shape () =
    let df = create_test_df () in
    let rows, cols = Dataframe.shape df in
    assert_equal 5 rows "Dataframe should have 5 rows" &&
    assert_equal 5 cols "Dataframe should have 5 columns"

let test_get_column_index () =
    let df = create_test_df () in
    assert_equal 1 (Dataframe.get_column_index df "Age") "Age column should have index 1"

let test_get_column () =
    let df = create_test_df () in
    let age_col = Dataframe.get_column df "Age" in
    let age_list = age_col |> Seq.filter (fun x -> x <> NULL) |> Seq.map (function 
    | INT_DATA i -> i 
    | _ -> failwith "Expected int") 
    |> List.of_seq in
    assert_true (age_list = [30; 25; 35; 28]) "Age column should contain [30, 25, 35, 28]"

let test_to_csv () =
    let df = create_test_df () in
    Dataframe.to_csv df "output_test.csv";
    let df2 = Dataframe.load_from_csv "output_test.csv" in
    let rows1, cols1 = Dataframe.shape df in
    let rows2, cols2 = Dataframe.shape df2 in
    assert_equal rows1 rows2 "CSV export should preserve row count" &&
    assert_equal cols1 cols2 "CSV export should preserve column count"

let test_to_json () =
    let df = create_test_df () in
    Dataframe.to_json df "output_test.json";
    let df2 = Dataframe.load_from_json "output_test.json" in
    let rows1, cols1 = Dataframe.shape df in
    let rows2, cols2 = Dataframe.shape df2 in
    assert_equal rows1 rows2 "JSON export should preserve row count" &&
    assert_equal cols1 cols2 "JSON export should preserve column count"

(* Lib Module Tests *)
let test_show_df () =
    let df = create_test_df () in
    try
      Lib.show_df df;
      true
    with _ -> false
  
let test_map () =
    let df = create_test_df () in
    let add_one = function
      | INT_DATA i -> INT_DATA (i + 1)
      | _ as v -> v in
    let mapped_df = Lib.map add_one "Age" df in
    let age_col = Dataframe.get_column mapped_df "Age" in
    let result = age_col |> Seq.filter (fun x -> x <> NULL) |> Seq.map (function 
      | INT_DATA i -> i 
      | _ -> failwith "Expected int") 
      |> List.of_seq in
    assert_true (result = [31; 26; 36; 29]) "Mapped ages should be original + 1"
  
let test_filter () =
    let df = create_test_df () in
    let is_over_30 = function
      | INT_DATA i -> i > 30
      | _ -> false in
    let filtered_df = Lib.filter is_over_30 "Age" df in
    assert_equal 1 (Dataframe.no_of_rows filtered_df) "Only one person (Bob) is over 30"
  
let test_mem () =
    let df = create_test_df () in
    assert_true (Lib.mem "Name" (STRING_DATA "John") df) "John should be in the dataframe" &&
    assert_true (not (Lib.mem "Name" (STRING_DATA "Unknown") df)) "Unknown should not be in the dataframe"
  
let test_fold_left_sum () =
    let df = create_test_df () in
    let add = fun acc x -> 
      match (acc, x) with
      | (INT_DATA acc_val, INT_DATA x_val) -> INT_DATA (acc_val + x_val)
      | _ -> acc in
    let result = Lib.fold_left "Age" add (INT_DATA 0) df in
    match result with
    | INT_DATA sum -> assert_equal 118 sum "Sum of ages should be 118"  (* 30 + 25 + 35 + 28 = 118 *)
    | _ -> assert_true false "Expected INT_DATA result"

let test_fold_right_sum () =
    let df = create_test_df () in
    let add = fun x acc -> 
        match (x, acc) with
        | (INT_DATA x_val, INT_DATA acc_val) -> INT_DATA (x_val + acc_val)
        | _ -> acc in
    let result = Lib.fold_right "Age" add (INT_DATA 0) df in
    match result with
    | INT_DATA sum -> assert_equal 118 sum "Sum of ages should be 118"  (* 30 + 25 + 35 + 28 = 118 *)
    | _ -> assert_true false "Expected INT_DATA result"
    
let test_normalize () =
    let df = create_test_df2 () in
    let normalized_df = Lib.normalize "Age" df in
    let age_col = Dataframe.get_column normalized_df "Age" in
    (* Check if the values are normalized (mean=0, std≈1) *)
    let values = age_col 
        |> Seq.filter (fun x -> x <> NULL) 
        |> Seq.map (function 
        | FLOAT_DATA f -> f 
        | _ -> failwith "Expected float") 
        |> List.of_seq in
    let sum = List.fold_left (+.) 0. values in
    let mean = sum /. float_of_int (List.length values) in
    assert_true (abs_float mean < 0.001) "Mean of normalized data should be close to 0"
    
let test_min_max_normalize () =
    let df = create_test_df2 () in
    let normalized_df = Lib.min_max_normalize "Age" df in
    let age_col = Dataframe.get_column normalized_df "Age" in
    let values = age_col 
        |> Seq.filter (fun x -> x <> NULL) 
        |> Seq.map (function 
        | FLOAT_DATA f -> f 
        | _ -> failwith "Expected float") 
        |> List.of_seq in
    let min_val = List.fold_left min max_float values in
    let max_val = List.fold_left max min_float values in
    assert_true (abs_float min_val < 0.001) "Min should be close to 0" &&
    assert_true (abs_float (max_val -. 1.0) < 0.001) "Max should be close to 1"
    
let test_imputena () =
    let df = create_test_df () in
    let imputed_df = Lib.imputena "Salary" df in
    let salary_col = Dataframe.get_column imputed_df "Salary" in
    let has_nulls = salary_col |> Seq.exists ((=) NULL) in
    assert_true (not has_nulls) "Should not have NULL values after imputation"
    
let test_join () =
    let df1 = create_test_df () in
    let df2 = create_join_df () in
    let joined_df = Lib.join df1 df2 "Department" in
    let _, cols1 = Dataframe.shape df1 in
    let _, cols_joined = Dataframe.shape joined_df in
    assert_true (cols_joined > cols1) "Joined dataframe should have more columns than original" &&
    (try
        let _ = Dataframe.get_column_index joined_df "Location" in
        true
    with _ -> false)
    
let test_sum () =
    let df = create_test_df () in
    let result = Lib.sum "Age" df in
    match result with
    | INT_DATA sum -> assert_equal 118 sum "Sum of ages should be 118"  (* 30 + 25 + 35 + 28 = 118 *)
    | _ -> assert_true false "Expected INT_DATA result"
    
let test_len () =
    let df = create_test_df () in
    let result = Lib.len "Age" df in
    match result with
    | INT_DATA len -> assert_equal 5 len "Should have 5 age values"
    | _ -> assert_true false "Expected INT_DATA result"
    
let test_mean () =
    let df = create_test_df2 () in
    let result = Lib.mean "Age" df in
    match result with
    | FLOAT_DATA mean -> assert_equal_float 34.2 mean 0.001 "Mean age should be 34.2" 
    | _ -> assert_true false "Expected FLOAT_DATA result"
    
let test_stddev () =
    let df = create_test_df2 () in
    let result = Lib.stddev "Age" df in
    match result with
    | FLOAT_DATA std -> 
        assert_equal_float 5.78 std 0.01 "Stddev should be close to 5.78"
    | _ -> assert_true false "Expected FLOAT_DATA result"

let test_iloc () = 
    let df = create_test_df2 () in
    let result_df = Lib.iloc 0 5 df in
    let (_, cols1) = Dataframe.shape df in
    let (rows2, cols2) = Dataframe.shape result_df in
    assert_equal rows2 6 "Expected 6 rows" &&
    assert_equal cols2 cols1 "Number of columns should not change"

let test_loc () = 
    let df = create_test_df2 () in
    let result_df = Lib.loc "Name" "John Doe" "Emily Wilson" df in
    let (_, cols1) = Dataframe.shape df in
    let (rows2, cols2) = Dataframe.shape result_df in
    assert_equal rows2 4 "Expected 4 rows" &&
    assert_equal cols2 cols1 "Number of columns should not change"


let () = 
    run_test "Load from CSV" test_load_from_csv;
    run_test "Load from JSON" test_load_from_json;
    run_test "No of rows" test_no_of_rows;
    run_test "Size" test_size;
    run_test "Shape" test_shape;
    run_test "Get column index" test_get_column_index;
    run_test "Get column" test_get_column;
    run_test "To CSV" test_to_csv;
    run_test "To JSON" test_to_json;

    run_test "Show DataFrame" test_show_df;
    run_test "Map" test_map;
    run_test "Filter" test_filter;
    run_test "Memory" test_mem;
    run_test "Fold left sum" test_fold_left_sum;
    run_test "Fold right sum" test_fold_right_sum;
    run_test "Normalize" test_normalize;
    run_test "Min-Max normalize" test_min_max_normalize;
    run_test "Impute NA" test_imputena;
    run_test "Join" test_join;
    run_test "Sum" test_sum;
    run_test "Length" test_len;
    run_test "Mean" test_mean;
    run_test "StdDev" test_stddev;
    run_test "iLoc" test_iloc;
    run_test "Loc" test_loc;


(* let main () = 
    begin

        let df = Dataframe.load_from_csv "testagg.csv" in

        let f row = 

            match List.nth row 1 with
            | STRING_DATA ("Electronics") -> true
            | _ -> false in

        let update_row row = 

            let new_row = Array.of_list row in

            let get_float data = 

                match data with 
                | FLOAT_DATA x -> x
                | _ -> failwith "Expected float value" in

            new_row.(2) <- FLOAT_DATA ((get_float new_row.(2)) +. 100.); 

            Array.to_list new_row in

        print_endline "\nUpdate row: \n";

        let new_df = Lib.update_row f update_row df in
        Lib.show_df new_df;

        print_endline "\n";

        print_endline "\nAggregate: \n";

        let df = Dataframe.load_from_csv "testagg.csv" in
        let mapping = [("Id", Lib.sum); ("Category", Lib.len); ("Price", Lib.mean)] in
        let new_df = Lib.groupByAggregate "Category" mapping df in
        Lib.show_df new_df;

        let df = Dataframe.load_from_csv "test1.csv" in
        print_endline "Original df: ";
        Lib.show_df df;

        print_endline "\nNew row added: \n";

        let new_row = [ STRING_DATA ("newname"); INT_DATA (45) ] in
        let new_df = Lib.add_row new_row df in
        Lib.show_df new_df;

        print_endline "\nDeletion of row: \n";

        let f row = 

            match row with 
            | [] -> false
            | h :: t -> h = STRING_DATA "Kandarp" in

        let new_df = Lib.delete_row f df in
        Lib.show_df new_df;

        let f = (function INT_DATA x -> FLOAT_DATA ((float_of_int x) +. 10.) | _ -> FLOAT_DATA (-1.)) in
        let new_df = Lib.map f "Age" df in

        print_endline "Map function (x + 10): ";
        Lib.show_df new_df;

        let new_df = Lib.normalize "Age" df in
        print_endline "Normalize: ";
        Lib.show_df new_df;

        let filt = (function INT_DATA x -> x <= 25 | _ -> false) in
        let new_df = Lib.filter filt "Age" df in 
        
        print_endline "Filter: ";
        Lib.show_df new_df;

        let el = INT_DATA (27) in 
        print_endline (string_of_bool (Lib.mem "Age" el df));

        print_endline "\nJoin: \n";

        let df1 = Dataframe.load_from_csv "test1.csv" and
        df2 = Dataframe.load_from_csv "test2.csv" in
        
        let joined_df = Lib.join df1 df2 "Name" in
        Lib.show_df joined_df;
    end

let _ = main () *)
