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

        let f = (function INT_DATA x -> FLOAT_DATA ((float_of_int x) +. 10.) | _ -> FLOAT_DATA (-1.)) in
        let new_df = Lib.map f "Age" df in

        print_endline "Map function (x + 10): ";
        Seq.iter Row.display_row new_df.rows;
        Dataframe.to_csv new_df "testk.csv";

        let new_df = Lib.normalize "Age" df in
        print_endline "Normalize: ";
        Seq.iter Row.display_row new_df.rows;

        let filt = (function INT_DATA x -> x <= 25 | _ -> false) in
        let new_df = Lib.filter filt "Age" df in 
        
        print_endline "Filter: ";
        Seq.iter Row.display_row new_df.rows;

        let el = INT_DATA (27) in 
        print_endline (string_of_bool (Lib.mem "Age" el df));

        print_endline "\nJoin: \n";

        let df1 = Dataframe.load_from_csv "test1.csv" and
        df2 = Dataframe.load_from_csv "test2.csv" in
        
        let joined_df = Lib.join df1 df2 "Name" in
        Seq.iter Row.display_row joined_df.rows;
    end

let _ = main ()
