module IntSet = Set.Make(Int);;

let debug = false

exception Invalid_end

let get_int_value_of_string str=
  match String.fold_right (fun char (acc_sum,acc_puissance) -> if '0'<=char && char<='9' then (acc_sum + ((int_of_char char) - (int_of_char '0')) * acc_puissance,acc_puissance*10) else (acc_sum,acc_puissance)) str (0,1) with
  |sum,_ -> (if debug then (print_int sum; print_string ("="^str^"\n"))); sum

let rec split_winning_numbers str strlen first_index winning_numbers= 
  if first_index + 1 >= strlen then raise Invalid_end 
  else 
  let char = str.[first_index] in
  match char with 
  | ' ' -> if str.[first_index + 1] = '|' 
    then (first_index + 2, winning_numbers)
    else split_winning_numbers str strlen (first_index+3) (IntSet.add (get_int_value_of_string (String.sub str (first_index+1) 2)) winning_numbers)
  | _ -> raise Invalid_end 


let rec split_numbers_string str strlen first_index numbers= 
  if first_index >= strlen then numbers 
  else 
  let char = str.[first_index] in
  match char with 
  | ' ' -> split_numbers_string str strlen (first_index+3) (IntSet.add (get_int_value_of_string (String.sub str (first_index+1) 2)) numbers)
  | _ -> raise Invalid_end 

let get_point_of_ticket str =
  let length = (String.length str) in
  let new_str = String.sub str 9 (length-9) in
  if debug then (print_string "line: "; print_string new_str; print_newline ());
  let offset, winning_numbers = split_winning_numbers new_str (length-9) 0 IntSet.empty in
  let new_str = String.sub str (offset+9) (length-offset-9) in
  if debug then (print_string "line: "; print_string new_str; print_newline ());
  let numbers = split_numbers_string new_str (length-offset-9) 0 IntSet.empty in
  let cardinal = IntSet.cardinal (IntSet.inter winning_numbers numbers) in
  if debug 
    then (let f i = print_int i; print_string " " in
      IntSet.iter f winning_numbers;
      print_newline ();
      IntSet.iter f numbers;
      print_newline ();)
    else ();
  if cardinal <= 1 then cardinal
  else int_of_float (2. ** (float_of_int (cardinal - 1)))


let () =
  if (Array.length Sys.argv != 2) 
    then 
      let () = print_string "Error: Missing filename argument in command call" in
      print_newline ()
    else 
      let file = Sys.argv.(1) in
      let lines = In_channel.input_lines (In_channel.open_bin file) in 
      let total = List.fold_left (fun acc x -> acc + (get_point_of_ticket x)) 0 lines in 
      let () = print_int total in print_newline ();;