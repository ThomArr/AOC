let debug = false

type coordinates = {x : int; y : int}
type number_coordinates = {x_start : int; x_end : int; y : int} 

module NumberCoordinates = struct
  type t = number_coordinates
  let compare nc1 nc2 =
    let c = compare nc1.x_start nc2.x_start in
    if c <> 0 then c
    else
      let c = compare nc1.x_end nc2.x_end in
      if c <> 0 then c
      else
        compare nc1.y nc2.y
end

module NumberCoordinatesMap = Map.Make(NumberCoordinates);;


let check_are_adjacent_symbol_previously_this_number (coord : number_coordinates) (symbols_coords: coordinates list) =
  let adjacent_coord_previous = {x=coord.x_start-1; y=coord.y} :: (List.init (coord.x_end - coord.x_start + 3) (fun i -> {x = coord.x_start - 1 + i; y = coord.y - 1})) in
  List.exists (fun coord_adj -> List.exists (fun coord_s -> coord_s = coord_adj) symbols_coords) adjacent_coord_previous

let rec delete_unnecessary_symbols_coord (current_coord_y : int) (symbols: coordinates list) =
  match symbols with 
  | hd :: tl when hd.y < current_coord_y-1 -> delete_unnecessary_symbols_coord current_coord_y tl
  | _ -> symbols

let rec get_sum_numbers_adjacent_and_not_already_counted_previously_this_symbol (coord : coordinates) (numbers_coords: (int*bool) NumberCoordinatesMap.t)=
  NumberCoordinatesMap.fold 
  (fun key value (new_numbers_coords,acc_sum) -> 
    if debug then (print_string "numbers_coords contents : ("; print_int key.x_start; print_string ", "; print_int key.x_end; print_string ", "; print_int key.y; print_string ") "; let number,_ = value in print_int number; print_newline ());
    if key.x_start-1 <= coord.x && coord.x <= key.x_end+1 && coord.y + 1 >= key.y && key.y >= coord.y -1 
      then 
        (match value with 
        | number_value, bool_count -> if bool_count 
          then 
            let () = if debug then (print_string ("sum: "); print_int number_value; print_newline ()) in 
            (NumberCoordinatesMap.add key (number_value,false) new_numbers_coords,acc_sum + number_value)
          else (NumberCoordinatesMap.add key value new_numbers_coords,acc_sum))
      else (NumberCoordinatesMap.add key value new_numbers_coords,acc_sum))
  numbers_coords (NumberCoordinatesMap.empty,0)

let rec delete_unnecessary_numbers_coord (current_coord_y : int) (numbers: (int*bool) NumberCoordinatesMap.t) =
  NumberCoordinatesMap.fold (fun key (number_value, bool_count) acc -> if key.y < current_coord_y-1 || not bool_count then acc else NumberCoordinatesMap.add key (number_value, bool_count) acc) numbers NumberCoordinatesMap.empty

let sum_with_first_element n tuple =
  match tuple with 
  |a,b,c -> n+a,b,c

let rec split_and_sum_string str strlen line column length numbers_coords symbols_coords = 
  if column >= strlen then (0, numbers_coords, symbols_coords)
  else 
    let char = if column + length - 1 = strlen then '.' else str.[column+length-1] in
    match char with 
    | '.' -> if length = 1
      then split_and_sum_string str strlen line (column + 1) 1 numbers_coords symbols_coords
      else let current_number_coords = {x_start=column; x_end=column+length-2; y=line} in
        let number_read = String.sub str column (length-1) in
        if check_are_adjacent_symbol_previously_this_number current_number_coords symbols_coords 
          then 
            let () = if debug then (print_string ("read and sum (.): "^number_read); print_newline ()); in
            sum_with_first_element (int_of_string (number_read)) (split_and_sum_string str strlen line (column+length) 1 numbers_coords symbols_coords)
          else let numbers_coords = NumberCoordinatesMap.add current_number_coords ((int_of_string (number_read), true)) numbers_coords in
            let () = if debug then (print_string ("read: "^number_read); print_newline ()); in
            split_and_sum_string str strlen line (column+length) 1 numbers_coords symbols_coords
    
    | n when '0' <= n && n <='9' -> split_and_sum_string str strlen line column (length+1) numbers_coords symbols_coords
  
    | _ -> 
      let number_read = if length = 1 then 0 else int_of_string (String.sub str column (length-1)) in
      let current_symbol_coord = {x=column+length-1; y=line} in
      let symbols_coords = current_symbol_coord :: symbols_coords in 
      if debug then (print_string "read and sum (symbole): "; print_int number_read; print_newline ());
      let numbers_coords, sum = get_sum_numbers_adjacent_and_not_already_counted_previously_this_symbol current_symbol_coord numbers_coords in
      sum_with_first_element (number_read + sum) (split_and_sum_string str strlen line (column+length) 1 numbers_coords symbols_coords)

let rec list_fold_left_i f i acc list =
  match list with
  |hd :: tl -> list_fold_left_i f (i+1) (f i acc hd) tl
  |[] -> acc

let rec print_coord_list list =
  match list with
  | hd :: [] -> print_string "x: "; print_int hd.x; print_string " y: "; print_int hd.y;print_newline ()
  | hd :: tl -> print_string "x: "; print_int hd.x; print_string " y: "; print_int hd.y; print_string "; "; print_newline (); print_coord_list tl
  | [] -> print_string "Empty coordinates list."; print_newline ()

let rec print_NumberCoordinatesMap ncm =
  let _ = NumberCoordinatesMap.fold 
  (fun key value acc -> 
    if acc = 0 
      then (print_string "x_start: "; print_int key.x_start; print_string " x_end: "; print_int key.x_end; print_string " y: "; print_int key.y; acc+1)
      else (print_string ";"; print_newline (); print_string "x_start: "; print_int key.x_start; print_string " x_end: "; print_int key.x_end; print_string " y: "; print_int key.y; acc))
  ncm 0 in ()

let () =
  if (Array.length Sys.argv != 2) 
    then 
      let () = print_string "Error: Missing filename argument in command call" in
      print_newline ()
    else 
      let file = Sys.argv.(1) in
      let lines = In_channel.input_lines (In_channel.open_bin file) in 
      let (total,numbers_coords ,symbols_coords) = list_fold_left_i 
        (fun i (sum_acc,numbers_coords_acc, symbols_coords_acc) x ->
          let numbers_coords_acc = if debug then numbers_coords_acc else delete_unnecessary_numbers_coord i numbers_coords_acc in
          let symbols_coords_acc = if debug then symbols_coords_acc else delete_unnecessary_symbols_coord i symbols_coords_acc in
          sum_with_first_element sum_acc (split_and_sum_string x (String.length x) i 0 1 numbers_coords_acc symbols_coords_acc)) 0 (0,NumberCoordinatesMap.empty,[]) lines in 
      let () = 
      if debug 
        then 
          (print_string "numbers_coords:\n";
          print_NumberCoordinatesMap numbers_coords; 
          print_newline ();
          print_string "symbols_coords:\n";
          print_coord_list symbols_coords;
          print_newline ());
      print_int total in print_newline ();;