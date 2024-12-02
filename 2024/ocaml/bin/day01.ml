let parse =
  List.fold_left
    (fun (first, second) x ->
      let parts = String.split_on_char ' ' x in
      ( (int_of_string @@ List.hd @@ parts) :: first
      , (int_of_string @@ List.hd @@ List.rev @@ parts) :: second ))
    ([], [])
    (Advent.read_lines_day ~d:1 ~i:Input)
;;

let part1 first second =
  List.fold_left2
    (fun acc x y -> acc + abs (x - y))
    0
    (List.sort compare first)
    (List.sort compare second)
;;

let part2 first second =
  List.fold_left
    (fun acc x -> acc + (x * List.length (List.filter (fun y -> x = y) second)))
    0
    first
;;

let _ =
  let first, second = parse in
  Advent.print_part1 ~s:(string_of_int @@ part1 first second);
  Advent.print_part2 ~s:(string_of_int @@ part2 first second)
;;
