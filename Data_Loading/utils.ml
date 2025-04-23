let strip str = 
  let trimmed = String.trim str in
  let l = String.length trimmed in
  if l < 2 then trimmed
  else 
    if (String.starts_with ~prefix:"\"" trimmed && String.ends_with ~suffix:"\"" trimmed) || 
      (String.starts_with ~prefix:"\'" trimmed && String.ends_with ~suffix:"\'" trimmed) then String.sub trimmed 1 (l-2)
    else trimmed