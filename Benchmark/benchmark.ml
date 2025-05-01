open Dataframe
open Row
open Lib
open Data_object
open Data_object.DataObject


let generate_dataset filename =
  let oc = open_out filename in
  (* Header row *)
  output_string oc "id,value,category,nullable\n";

  (* Type row *)
  output_string oc "int,float,string,float\n";

  (* Data rows *)
  for i = 0 to 99_999 do
    let id = i in
    let value = Random.float 100.0 in
    let category = match Random.int 3 with
      | 0 -> "A"
      | 1 -> "B"
      | _ -> "C"
    in
    let nullable =
      if Random.float 1.0 > 0.8 then ""  (* simulate null value *)
      else string_of_float (Random.float 50.0)
    in
    Printf.fprintf oc "%d,%.3f,%s,%s\n" id value category nullable
  done;
  close_out oc;

module Benchmark = struct
  let measure f name =
    let start_time = Unix.gettimeofday () in
    let stat_before = Gc.quick_stat () in
    let result = f () in
    let end_time = Unix.gettimeofday () in
    let stat_after = Gc.quick_stat () in
    let time_taken = end_time -. start_time in
    let memory_used_kb =
      (float_of_int (stat_after.Gc.live_words - stat_before.Gc.live_words)) *. (float_of_int (Sys.word_size / 8)) /. 1024.0
    in
    Printf.printf "%-20s | Time: %.6fs | Memory: %.2f KB\n" name time_taken memory_used_kb;
    result
end




let () = 
  let _ = generate_dataset "test.csv" in
  let df = Dataframe.load_from_csv "test.csv" in

  let f1 () = 
    let _ = Dataframe.load_from_csv "test.csv" in () in
  Benchmark.measure f1 "Load CSV";

  let mul_2 = function
  | INT_DATA i -> INT_DATA (i * 2)
  | _ as v -> v in
  let f2 () = 
    let _ = Lib.map mul_2 "value" df in () in
  Benchmark.measure f2 "Map" ;

  let is_over_50 = function
  | INT_DATA i -> i > 50
  | _ -> false in
  let f3 () = 
    let _ = Lib.filter is_over_50 "value" df in () in
  Benchmark.measure f3 "Filter";

  let add = fun x acc -> 
    match (x, acc) with
    | (INT_DATA x_val, INT_DATA acc_val) -> INT_DATA (x_val + acc_val)
    | _ -> acc in
  let f4 () = 
    let _ = Lib.fold_left "value" add (INT_DATA 0) df in () in
  Benchmark.measure f4 "Fold";

  let f5 () = 
    let _ = Lib.normalize "value" df in () in
  Benchmark.measure f5 "Normalize";

  let f6 () = 
    let _ = Lib.min_max_normalize "value" df in () in
  Benchmark.measure f6 "MinMax Normalize";

  let f7 () = 
    let _ = Lib.imputena "nullable" df in () in
  Benchmark.measure f7 "Impute NA";

  let f8 () = 
    let _ = Lib.fillna "nullable" (FLOAT_DATA 0.) df in () in
  Benchmark.measure f8 "Fill NA";

  let f9 () = 
    let _ = Lib.iloc 1345 5431 df in () in
  Benchmark.measure f9 "iLoc";

  let f10 () =
    let _ = Lib.groupByAggregate "category" [("value", Lib.mean); ("nullable", Lib.sum)] df in () in
  Benchmark.measure f10 "GroupBy-Agg"
