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

let rec split_string str strlen line column length numbers_coords symbols_coords = 
  if column >= strlen then  numbers_coords, symbols_coords
  else 
    let char = if column + length - 1 = strlen then '.' else str.[column+length-1] in
    match char with 
    | n when '0' <= n && n <='9' -> split_string str strlen line column (length+1) numbers_coords symbols_coords
  
    | s -> 
      let symbols_coords =
        if s = '*'
          then 
            let current_symbol_coord = {x=column+length-1; y=line} in
            current_symbol_coord :: symbols_coords
          else symbols_coords 
      in 
      let numbers_coords =
        if length = 1
          then numbers_coords
          else NumberCoordinatesMap.add {x_start = column; x_end = column+length-2; y= line} (int_of_string (String.sub str column (length-1))) numbers_coords
      in
      split_string str strlen line (column+length) 1 numbers_coords symbols_coords

let rec get_and_sum_all_ratio numbers_coords symbols_coords =
  match symbols_coords with
  |{x;y} :: tl -> 
    let gears = NumberCoordinatesMap.filter (fun key _ -> key.x_start-1 <= x && x <= key.x_end+1 && key.y-1 <= y && y <= key.y+1) numbers_coords in
    if NumberCoordinatesMap.cardinal (gears) != 2 
      then get_and_sum_all_ratio numbers_coords tl
      else 
        let () = 
          if debug 
            then (
              let _ = NumberCoordinatesMap.fold 
              (fun key value acc -> print_int value; if acc = 0 then print_string "*"; acc+1)
              gears 0 in 
              print_newline ();)
        in 
        NumberCoordinatesMap.fold (fun key value acc-> value*acc) gears 1 + get_and_sum_all_ratio numbers_coords tl
  |[] -> 0
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
      let (numbers_coords ,symbols_coords) = list_fold_left_i 
        (fun i (numbers_coords_acc, symbols_coords_acc) x ->
          split_string x (String.length x) i 0 1 numbers_coords_acc symbols_coords_acc) 0 (NumberCoordinatesMap.empty, []) lines in 
      let () = 
      if debug 
        then 
          (print_string "numbers_coords:\n";
          print_NumberCoordinatesMap numbers_coords; 
          print_newline ();
          print_string "symbols_coords:\n";
          print_coord_list symbols_coords;
          print_newline ());
      print_int (get_and_sum_all_ratio numbers_coords symbols_coords) in print_newline ();;