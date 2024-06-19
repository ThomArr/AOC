exception Number_Not_Found;;

let rec get_first_number_on_line str str_len i=
  if str_len <= i then raise Number_Not_Found
  else 
    let c = str.[i] in 
    if ('0' <= c && c <= '9' ) 
      then int_of_char c - int_of_char '0'
      else get_first_number_on_line str str_len (i+1)


let rec get_last_number_on_line str str_len i=
  if str_len <= -1 then raise Number_Not_Found
  else 
    let c = str.[i] in 
    if ('0' <= c && c <= '9' ) 
      then int_of_char c - int_of_char '0'
      else get_last_number_on_line str str_len (i-1)

let get_number_of_line str =
  let length = String.length str in
  (get_first_number_on_line str length 0) * 10 + get_last_number_on_line str length (length-1)


let () =
  if (Array.length Sys.argv != 2) 
    then 
      let () = print_string "Error: Missing filename argument in command call" in
      print_newline ()
    else 
      let file = Sys.argv.(1) in
      let lines = In_channel.input_lines (In_channel.open_bin file) in 
      let total = List.fold_left (fun acc x -> acc + (get_number_of_line x)) 0 lines in 
      let () = print_int total in print_newline ();;