open Advent

let first = ref []
let second = ref []

let parse =
  List.iter
    (fun x ->
      let parts = String.split_on_char ' ' x in
      first := (parts |> List.hd |> int_of_string) :: !first;
      second := (parts |> List.rev |> List.hd |> int_of_string) :: !second)
    (read_lines "01-input.txt")
;;

let part1 =
  let res =
    List.map2
      (fun x y -> abs (x - y))
      (List.sort compare !first)
      (List.sort compare !second)
  in
  List.fold_left ( + ) 0 res |> string_of_int
;;

let part2 =
  let res =
    List.map (fun x -> x * List.length (List.filter (fun y -> x = y) !second)) !first
  in
  List.fold_left ( + ) 0 res |> string_of_int
;;

let _ =
  parse;
  print_endline part1;
  print_endline part2
;;
