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

let rec add_x_to_the_first_i_elements x i list =
  match list with
  | l when i = 0 -> l
  | hd :: tl -> hd + x :: add_x_to_the_first_i_elements x (i-1) tl 
  | [] -> x :: add_x_to_the_first_i_elements x (i-1) []

let get_cardinal_winning_numbers_of_ticket str =
  let length = (String.length str) in
  let new_str = String.sub str 9 (length-9) in
  if debug then (print_string "line: "; print_string new_str; print_newline ());
  let offset, winning_numbers = split_winning_numbers new_str (length-9) 0 IntSet.empty in
  let new_str = String.sub str (offset+9) (length-offset-9) in
  if debug then (print_string "line: "; print_string new_str; print_newline ());
  let numbers = split_numbers_string new_str (length-offset-9) 0 IntSet.empty in
  if debug 
    then (let f i = print_int i; print_string " " in
      IntSet.iter f winning_numbers;
      print_newline ();
      IntSet.iter f numbers;
      print_newline ();)
    else ();
  IntSet.cardinal (IntSet.inter winning_numbers numbers)

let rec print_int_list l=
  match l with
  | hd :: [] -> print_int hd; print_string "."; print_newline ()
  | hd :: tl -> print_int hd; print_string "; "; print_int_list tl
  | [] -> print_string "Empty list."; print_newline ()

let () =
  if (Array.length Sys.argv != 2) 
    then (
      print_string "Error: Missing filename argument in command call";
      print_newline ();)
    else 
      let file = Sys.argv.(1) in
      let lines = In_channel.input_lines (In_channel.open_bin file) in 
      let (_,total) = List.fold_left 
        (fun (list_count_each_tickets,count_total_tickets) x -> 
          if debug then(
            print_int_list list_count_each_tickets;
            print_int count_total_tickets;
            print_newline ();
            print_newline ());
          let cardinal_winning_numbers_of_ticket = (get_cardinal_winning_numbers_of_ticket x) in 
          match list_count_each_tickets with
          |hd :: tl -> (add_x_to_the_first_i_elements (hd+1) cardinal_winning_numbers_of_ticket tl, 1 + count_total_tickets + hd)
          | [] -> (add_x_to_the_first_i_elements 1 cardinal_winning_numbers_of_ticket []), 1 + count_total_tickets)
        ([],0) lines in 
      let () = print_int total in print_newline ();;