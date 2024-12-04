open Advent.Helpers

let part1 =
  List.fold_left
    (fun acc reg_match ->
       acc
       + ((int_of_string @@ Re.Group.get reg_match 1)
          * (int_of_string @@ Re.Group.get reg_match 2)))
    0
  << Re.all
     @@ Re.compile
          Re.(
            seq
              [ str "mul("
              ; group (repn digit 1 @@ Some 3)
              ; str ","
              ; group (repn digit 1 @@ Some 3)
              ; str ")"
              ])

let part2 =
  fst
  << List.fold_left
       (fun (acc, enabled) -> function
          | `Delim delim -> acc, Re.Group.get delim 0 = "do()"
          | `Text str -> if enabled then acc + part1 str, enabled else acc, enabled)
       (0, true)
  << Re.split_full @@ Re.compile @@ Re.(alt [ str "do()"; str "don't()" ])

let _ =
  let parse = Advent.read_file_day ~d:3 ~i:Input in
  Advent.print_part1 ~s:(string_of_int @@ part1 parse);
  Advent.print_part2 ~s:(string_of_int @@ part2 parse)
