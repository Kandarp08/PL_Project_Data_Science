open Dataframe
open Row
open Lib
open Data_object
open Data_object.DataObject

let main () = 
    begin
        let df = Dataframe.load_from_csv "test1.csv" in
        print_endline "Original df: ";
        Seq.iter Row.display_row df.rows;

        let f = (function INT_DATA x -> INT_DATA (x + 10) | _ -> INT_DATA (-1)) in
        let new_df = Lib.map df f "Age" in

        print_endline "Map function (x + 10): ";
        Seq.iter Row.display_row new_df.rows;

        let new_df = Lib.normalize df "Age" in
        print_endline "Normalize: ";
        Seq.iter Row.display_row new_df.rows;

        let filt = (function INT_DATA x -> x <= 25 | _ -> false) in
        let new_df = Lib.filter df filt "Age" in 
        
        print_endline "Filter: ";
        Seq.iter Row.display_row new_df.rows;

        let el = INT_DATA (27) in 
        print_endline (string_of_bool (Lib.mem df "Age" el));

        print_endline "\nJoin: \n";

        let df1 = Dataframe.load_from_csv "test1.csv" and
        df2 = Dataframe.load_from_csv "test2.csv" in
        
        let joined_df = Lib.join df1 df2 "Name" in
        Seq.iter Row.display_row joined_df.rows;
    end

let _ = main ()
