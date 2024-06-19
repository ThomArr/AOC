module StringMap = Map.Make(String);;

let debug = false

let set_value_to_key color dico_count new_value=
  if StringMap.exists (fun key value -> key = color) dico_count 
    then StringMap.update color (fun value -> match value with 
                                            |Some(value) -> Some (if value > new_value then value else new_value)
                                            |None -> None) dico_count
    else StringMap.add color new_value dico_count

let rec split_string str strlen last_number_read first_index length dico_count= 
  if first_index >= strlen then dico_count 
  else 
  let char = if first_index + length >= strlen then ';' else str.[first_index + length] in
  match char with 
  | ' ' -> let number = String.sub str first_index length in 
          let () = (print_string ("int_of_string: "^number); print_newline ()) in
          split_string str strlen (int_of_string number) (first_index + length + 1) 1 dico_count

  | ',' -> let color = String.sub str first_index length in 
          let dico_count = set_value_to_key color dico_count last_number_read in
          split_string str strlen 0 (first_index + length + 2) 1 dico_count

  | ';' -> let color = String.sub str first_index length in 
          let dico_count = set_value_to_key color dico_count last_number_read in
            split_string str strlen 0 (first_index + length + 2) 1 dico_count
  | _ -> split_string str strlen last_number_read first_index (length+1) dico_count

let rec get_offset_number_of_game str strlen first_index length= 
  if first_index + length >= strlen then raise Out_of_memory
  else 
  match str.[first_index + length] with
  | ':' -> length
  | _ -> get_offset_number_of_game str strlen first_index (length+1)

let get_the_power_of_a_set str =
  let offset = get_offset_number_of_game str (String.length str) 5 1 in
  let length = (String.length str) - (offset + 7) in
  let new_str = String.sub str (offset + 7) length in
  if debug then (print_string "line: "; print_string new_str; print_newline ());
  let dico_count = split_string new_str length 0 0 1 StringMap.empty in
  StringMap.fold (fun key value acc -> acc * value) dico_count 1


let () =
  if (Array.length Sys.argv != 2) 
    then 
      let () = print_string "Error: Missing filename argument in command call" in
      print_newline ()
    else 
      let file = Sys.argv.(1) in
      let lines = In_channel.input_lines (In_channel.open_bin file) in 
      let total = List.fold_left (fun acc x -> acc + (get_the_power_of_a_set x)) 0 lines in 
      let () = print_int total in print_newline ();;