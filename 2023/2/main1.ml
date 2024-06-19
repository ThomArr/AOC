module StringMap = Map.Make(String);;

let debug = true

let initial_config = StringMap.of_seq @@ List.to_seq [
    ("red",12);
    ("green", 13);
    ("blue", 14);
  ];;

let add_value_to_key color dico_count value_to_add=
  if StringMap.exists (fun key value -> key = color) dico_count 
    then StringMap.update color (fun value -> match value with 
                                            |Some(value) -> Some (value + value_to_add)
                                            |None -> None) dico_count
    else StringMap.add color value_to_add dico_count

let rec split_and_check_string str strlen last_number_read first_index length dico_count= 
  if first_index >= strlen then true 
  else 
  let char = if first_index + length >= strlen then ';' else str.[first_index + length] in
  match char with 
  | ' ' -> let number = String.sub str first_index length in 
          let () = (print_string ("int_of_string: "^number); print_newline ()) in
          split_and_check_string str strlen (int_of_string number) (first_index + length + 1) 1 dico_count

  | ',' -> let color = String.sub str first_index length in 
          let dico_count = add_value_to_key color dico_count last_number_read in
          split_and_check_string str strlen 0 (first_index + length + 2) 1 dico_count

  | ';' -> let color = String.sub str first_index length in 
          let dico_count = add_value_to_key color dico_count last_number_read in
          if debug 
            then  let () = StringMap.fold 
                          (fun key value acc -> print_string (key^": "); 
                                                print_int value; 
                                                print_string " <= "; 
                                                print_int (StringMap.find key initial_config); 
                                                print_newline ()) 
                          dico_count () in
                  print_newline ()
            else ();
          if StringMap.exists (fun key value -> match StringMap.find_opt key initial_config with | Some maximum -> value > maximum | None -> false ) dico_count
            then false
            else split_and_check_string str strlen 0 (first_index + length + 2) 1 StringMap.empty
  | _ -> split_and_check_string str strlen last_number_read first_index (length+1) dico_count

let rec get_offset_and_number_of_game str strlen first_index length= 
  if first_index + length >= strlen then raise Out_of_memory
  else 
  match str.[first_index + length] with
  | ':' -> (String.sub str first_index length, length)
  | _ -> get_offset_and_number_of_game str strlen first_index (length+1)

let check_is_there_enough_cube_to_play_this_party str =
  let (number_of_game,offset) =get_offset_and_number_of_game str (String.length str) 5 1 in
  let length = (String.length str) - (offset + 7) in
  let new_str = String.sub str (offset + 7) length in
  if debug then (print_string "line: "; print_string new_str; print_newline ());
  let res = split_and_check_string new_str length 0 0 1 StringMap.empty in
  if res then int_of_string number_of_game else 0



let () =
  if (Array.length Sys.argv != 2) 
    then 
      let () = print_string "Error: Missing filename argument in command call" in
      print_newline ()
    else 
      let file = Sys.argv.(1) in
      let lines = In_channel.input_lines (In_channel.open_bin file) in 
      let total = List.fold_left (fun acc x -> acc + (check_is_there_enough_cube_to_play_this_party x)) 0 lines in 
      let () = print_int total in print_newline ();;