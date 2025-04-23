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
    end

let _ = main ()
