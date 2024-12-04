open Advent.Helpers

let is_valid_sequence lst =
  let rec zip_with_next = function
    | [] | [ _ ] -> []
    | x :: (y :: _ as tl) -> (int_of_string x, int_of_string y) :: zip_with_next tl
  in
  let direction =
    match zip_with_next lst with
    | (a, b) :: _ when a < b -> 1
    | _ -> -1
  in
  List.for_all (fun (x, y) -> direction * (y - x) >= 1 && direction * (y - x) <= 3)
  @@ zip_with_next lst

let part1 = List.length << List.filter is_valid_sequence

let part2 =
  List.length
  << List.filter (fun lst ->
    is_valid_sequence lst
    || Base.List.existsi lst ~f:(fun i _ ->
      let lst' = List.filteri (fun i' _ -> i' <> i) lst in
      is_valid_sequence lst'))

let _ =
  let parse =
    List.map (fun s -> String.split_on_char ' ' s) (Advent.read_lines_day ~d:2 ~i:Input)
  in
  Advent.print_part1 ~s:(string_of_int @@ part1 parse);
  Advent.print_part2 ~s:(string_of_int @@ part2 parse)
