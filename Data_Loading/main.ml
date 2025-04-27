open Dataframe
open Row

let main () = 
    begin
        let df = Dataframe.load_from_json "test.json" in
        Seq.iter Row.display_row df.rows;
        Dataframe.to_csv df "test2.csv";
    end

let _ = main ()
