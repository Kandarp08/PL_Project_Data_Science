open Dataframe
open Row

let main () = 
    begin
        let df = Dataframe.load_from_csv "test.csv" in
        let _ = Dataframe.to_json df "test3.json" in
        let df = Dataframe.load_from_json "test3.json" in
        Seq.iter Row.display_row df.rows;
    end

let _ = main ()
