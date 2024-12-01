open Advent

let parse =
  List.fold_left
    (fun (first, second) x ->
      let parts = String.split_on_char ' ' x in
      ( (parts |> List.hd |> int_of_string) :: first
      , (parts |> List.rev |> List.hd |> int_of_string) :: second ))
    ([], [])
    (Advent.read_lines_day ~d:1 ~i:Input)
;;

let part1 first second =
  let res =
    List.map2
      (fun x y -> abs (x - y))
      (List.sort compare first)
      (List.sort compare second)
  in
  List.fold_left ( + ) 0 res |> string_of_int
;;

let part2 first second =
  let res =
    List.map (fun x -> x * List.length (List.filter (fun y -> x = y) second)) first
  in
  List.fold_left ( + ) 0 res |> string_of_int
;;

let _ =
  let first, second = parse in
  print_part1 ~s:(part1 first second);
  print_part2 ~s:(part2 first second)
;;
