let strip str = 
  let trimmed = String.trim str in
  let l = String.length trimmed in
  if l < 2 then trimmed
  else 
    if (String.starts_with ~prefix:"\"" trimmed && String.ends_with ~suffix:"\"" trimmed) || 
      (String.starts_with ~prefix:"\'" trimmed && String.ends_with ~suffix:"\'" trimmed) then String.sub trimmed 1 (l-2)
    else trimmed


type json_object = (string * value) list
and 
value = 
  OBJECT of json_object
| ARRAY of value list
| STRING_VAL of string
| NUMBER of string
| BOOLEAN_VAL of bool
| NULL_VAL

module JSON = struct
  type t = json_object

  let rec skip_whitespace str i = 
    if i >= String.length str then i
    else match str.[i] with
      ' ' 
    | '\n'
    | '\t'
    | '\r' -> skip_whitespace str (i+1)
    | _ -> i

  let eat_comma str i = 
    if i >= String.length str || str.[i] != ',' then failwith "Expected comma"
    else (i+1)

  let eat_colon str i = 
    if i >= String.length str || str.[i] != ':' then failwith "Expected colon"
    else (i+1)

  let parse_string str i = 
    if i >= String.length str || str.[i] != '\"' then failwith "Expected \""
    else
      let rec iter str i acc escaped = 
        if i >= String.length str then failwith "Unexpected end of string"
        else if escaped then
          let escaped_char = match str.[i] with
            | '\"' -> "\""
            | '\\' -> "\\"
            | '/' -> "/"
            | 'b' -> "\b"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | 'u' -> 
                if i + 4 >= String.length str then failwith "Invalid Unicode escape sequence"
                else
                  let hex = String.sub str (i+1) 4 in
                  String.make 1 (Char.chr (int_of_string ("0x" ^ hex)))
            | _ -> String.make 1 str.[i]
          in
          iter str (i+1) (acc ^ escaped_char) false
        else if str.[i] = '\\' then
          iter str (i+1) acc true
        else if str.[i] = '\"' then
          (i+1, acc)
        else
          iter str (i+1) (acc ^ (String.make 1 str.[i])) false
      in
      iter str (i+1) "" false

  let parse_number str i = 
    let rec consume_digits str i acc =
      if i >= String.length str then (i, acc)
      else if str.[i] >= '0' && str.[i] <= '9' then
        consume_digits str (i+1) (acc ^ (String.make 1 str.[i]))
      else (i, acc)
    in
  
    let (i, acc) = 
      if i < String.length str && str.[i] = '-' then (i+1, "-")
      else (i, "") 
    in
    
    (* Integer part *)
    let (i, acc) = 
      if i >= String.length str then failwith "Unexpected end of number"
      else if str.[i] = '0' then (i+1, acc ^ "0")
      else if str.[i] >= '1' && str.[i] <= '9' then
        let (i', digits) = consume_digits str i "" in
        (i', acc ^ digits)
      else failwith "Invalid number format"
    in
    
    (* Fractional part *)
    let (i, acc) = 
      if i < String.length str && str.[i] = '.' then
        let i = i + 1 in
        if i >= String.length str || str.[i] < '0' || str.[i] > '9' then
          failwith "Expected digit after decimal point"
        else
          let (i', digits) = consume_digits str i "" in
          (i', acc ^ "." ^ digits)
      else (i, acc)
    in
    
    (* Exponent part *)
    let (i, acc) =
      if i < String.length str && (str.[i] = 'e' || str.[i] = 'E') then
        let i = i + 1 in
        let (i, acc) = 
          if i < String.length str && (str.[i] = '+' || str.[i] = '-') then
            (i + 1, acc ^ "e" ^ (String.make 1 str.[i]))
          else
            (i, acc ^ "e")
        in
        if i >= String.length str || str.[i] < '0' || str.[i] > '9' then
          failwith "Expected digit in exponent"
        else
          let (i', digits) = consume_digits str i "" in
          (i', acc ^ digits)
      else (i, acc)
    in
    
    (i, acc)

  let parse_keyword name str i = 
    let l = String.length name in
    if i + l > String.length str then 
      failwith ("Expected " ^ name ^ " but got end of input")
    else if (String.sub str i l) = name then i+l
    else failwith ("Expected " ^ name ^ " but got something else")

  let rec parse_value str i = 
    let i = skip_whitespace str i in
    if i >= String.length str then failwith "Unexpected end of input"
    else
      let parsed = match str.[i] with 
        '\"' -> let (i, s) = parse_string str i in (i, STRING_VAL (s))
      |  '[' -> let (i, a) = parse_array str i in (i, ARRAY (a))
      | '{' -> let (i, o) = parse_object str i in (i, OBJECT (o))
      | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> 
          let (i2, n) = parse_number str i in (i2, NUMBER (n)) 
      | 'f' -> let i = parse_keyword "false" str i in (i, BOOLEAN_VAL (false))
      | 't' -> let i = parse_keyword "true" str i in (i, BOOLEAN_VAL (true))
      | 'n' -> let i = parse_keyword "null" str i in (i, NULL_VAL)
      | _ -> failwith ("Unexpected token: " ^ (String.make 1 str.[i]))
      in
      let (i, v) = parsed in
      (skip_whitespace str i, v)

  and parse_array str i = 
    if i >= String.length str || str.[i] != '[' then 
      failwith "Expected '['"
    else
      let rec iter str i acc = 
        let i = skip_whitespace str i in
        if i >= String.length str then 
          failwith "Unterminated array"
        else if str.[i] = ']' then 
          (i+1, List.rev acc)
        else
          let i = if acc = [] then i else eat_comma str i in
          let (i, v) = parse_value str i in
          iter str i (v :: acc)
      in
      iter str (i+1) []

  and parse_object str i = 
    if i >= String.length str || str.[i] != '{' then 
      failwith "Expected '{'"
    else
      let rec iter str i acc = 
        let i = skip_whitespace str i in
        if i >= String.length str then
          failwith "Unterminated object"
        else if str.[i] = '}' then
          (i+1, acc)
        else
          let i = if acc = [] then i else eat_comma str i in
          let i = skip_whitespace str i in
          let (i, key) = parse_string str i in
          let i = skip_whitespace str i in
          let i = eat_colon str i in
          let (i, v) = parse_value str i in
          iter str i (acc @ [(key, v)])
      in
      iter str (i+1) []

  let parse str = 
    let (_, value) = parse_value str 0 in
    match value with
    | OBJECT obj -> obj
    | _ -> failwith "Expected JSON object at top level"
end