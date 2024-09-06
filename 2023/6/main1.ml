let debug = false

let split_string str =
  String.split_on_char ' ' str |> List.filter (fun s -> String.length s > 0 && not (String.equal "Time:" s) && not (String.equal "Distance:" s))

let get_number_ways_to_win time distance =
  let rec aux time_hold_button time_go_of_button =
    let millimeters = time_hold_button * time_go_of_button in
    (if millimeters > distance then 1 else 0)
    + 
    (if time_go_of_button <= 0 
      then 0
      else aux (time_hold_button + 1) (time_go_of_button -1))
  in let res = aux 0 time 
  in let () = if debug then (print_int res; print_newline ()) in 
  res

let () =
  if (Array.length Sys.argv != 2) 
    then 
      let () = print_string "Error: Missing filename argument in command call" in
      print_newline ()
    else 
      let file = Sys.argv.(1) in
      let lines = In_channel.input_lines (In_channel.open_bin file) in 
      let times = split_string (List.nth lines 0) in
      let distances = split_string (List.nth lines 1) in 
      let () = if debug then (List.iter (fun n -> print_string n; print_string "; ") times; print_newline (); List.iter (fun n -> print_string n; print_string "; ") distances; print_newline ()) in
      let total = List.fold_left2 (fun acc time distance -> acc * (get_number_ways_to_win (int_of_string time) (int_of_string distance))) 1 times distances in 
      let () = print_int total in print_newline ();;