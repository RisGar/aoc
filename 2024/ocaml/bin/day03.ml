let parse = Advent.read_file_day ~d:03 ~i:Input

let part1 input =
  let regex =
    Re.compile
      Re.(
        seq
          [ str "mul("
          ; group (repn digit 1 (Some 3))
          ; str ","
          ; group (repn digit 1 (Some 3))
          ; str ")"
          ])
  in
  List.fold_left
    (fun acc reg_match ->
      acc
      + ((int_of_string @@ Re.Group.get reg_match 1)
         * (int_of_string @@ Re.Group.get reg_match 2)))
    0
  @@ Re.all regex input
;;

let part2 input =
  fst
  @@ List.fold_left
       (fun (acc, enabled) str ->
         match str with
         | str when String.starts_with ~prefix:"n't()" str -> acc, false
         | str when String.starts_with ~prefix:"()" str -> acc + part1 str, true
         | str ->
           (match enabled with
            | false -> acc, enabled
            | true -> acc + part1 str, enabled))
       (0, true)
  @@ Str.split (Str.regexp "do") input
;;

let _ =
  Advent.print_part1 ~s:(string_of_int @@ part1 parse);
  Advent.print_part2 ~s:(string_of_int @@ part2 parse)
;;
