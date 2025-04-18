open Dataframe
open Row

let main () = 
    begin
        let df = Dataframe.load_from_csv "test.csv" in
        Seq.iter Row.display_row df.rows;
    end

let _ = main ()
