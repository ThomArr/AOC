let debug = false

let split_string str =
  String.split_on_char ' ' str |> List.filter (fun s -> String.length s > 0 && not (String.equal "Time:" s) && not (String.equal "Distance:" s)) |> List.fold_left (fun s acc -> s ^ acc) ""

let get_first_ways_to_win time distance =
  let rec aux time_hold_button time_go_of_button =
    if (time_go_of_button < 0) 
      then -1 
      else
      let millimeters = time_hold_button * time_go_of_button in
      (if millimeters > distance 
        then time_hold_button 
        else aux (time_hold_button + 1) (time_go_of_button - 1))
  in let res = aux 0 time 
  in let () = if debug then (print_int res; print_newline ()) in 
  res

let get_last_ways_to_win time distance =
  let rec aux time_hold_button time_go_of_button =
    if (time_hold_button < 0) 
      then -1 
      else
      let millimeters = time_hold_button * time_go_of_button in
      (if millimeters > distance 
        then time_hold_button 
        else aux (time_hold_button - 1) (time_go_of_button + 1))
  in let res = aux time 0
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
      let time = split_string (List.nth lines 0) |> int_of_string in
      let distance = split_string (List.nth lines 1) |> int_of_string in 
      let () = if debug then (print_int time; print_string "; "; print_newline (); print_int distance; print_string "; "; print_newline ()) in
      let total = get_last_ways_to_win time distance - get_first_ways_to_win time distance + 1 in 
      let () = print_int total in print_newline ();;