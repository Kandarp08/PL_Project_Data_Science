open Dataframe
open Row
open Lib
open Data_object
open Data_object.DataObject

let main () = 
    begin
        let df = Dataframe.load_from_csv "test1.csv" in
        let f = (function INT_DATA x -> INT_DATA (x + 10) | _ -> INT_DATA (-1)) in
        let new_df = Lib.map df f "Age" in
        Seq.iter Row.display_row new_df.rows;
    end

let _ = main ()
