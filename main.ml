open Dataframe
open Row
open Lib
open Data_object
open Data_object.DataObject

let main () = 
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
        let mapping = [("Id", Lib.sum); ("Category", Lib.len); ("Price", Lib.sum)] in
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

let _ = main ()
