open Advent.Helpers

let parse =
  let rules, updates =
    List.partition (fun line -> String.contains line '|')
    @@ List.filter (fun line -> String.length line > 0)
    @@ Advent.read_lines_day ~d:5 ~i:Input
  in
  ( Base.List.map rules ~f:(fun rule ->
      match String.split_on_char '|' rule with
      | x :: y :: _ -> int_of_string x, int_of_string y
      | _ -> failwith "Parsing failed")
  , List.map (fun line -> List.map int_of_string @@ String.split_on_char ',' line) updates
  )

let get_middle_number lst = List.nth lst (List.length lst / 2)
let pre_rules rules num = List.filter (fun (x, _) -> x = num) rules
let post_rules rules num = List.filter (fun (_, y) -> y = num) rules

let is_correct rules update =
  Base.List.for_alli update ~f:(fun i num ->
    let num_correct n op =
      0 = List.length @@ List.filteri (fun j found -> found = n && op j i) @@ update
    in
    (List.for_all (fun (_, y) -> num_correct y ( < )) @@ pre_rules rules num)
    && (List.for_all (fun (x, _) -> num_correct x ( > )) @@ post_rules rules num))

let part1 (rules, updates) =
  List.fold_left (fun acc lst -> acc + get_middle_number lst) 0
  @@ List.filter (is_correct rules) updates

let swap n op ~line ~i =
  let pred = Base.List.findi !line ~f:(fun j found -> found = n && op j i) in
  match pred with
  | Some (j, _) ->
    let i' = List.nth !line i in
    line
    := Base.List.mapi !line ~f:(fun nth x ->
         if nth = i then List.nth !line j else if nth = j then i' else x)
  | None -> ()

let process_line rules line =
  let line = ref line in
  while not (is_correct rules !line) do
    Base.List.iteri !line ~f:(fun i num ->
      List.iter (fun (_, y) -> swap y ( < ) ~line ~i) @@ pre_rules rules num;
      List.iter (fun (x, _) -> swap x ( > ) ~line ~i) @@ post_rules rules num)
  done;
  !line

let part2 (rules, updates) =
  List.fold_left (fun acc lst -> acc + get_middle_number lst) 0
  @@ List.map (process_line rules)
  @@ List.filter (not << is_correct rules) updates

let _ =
  Advent.print_part1 ~s:(string_of_int @@ part1 parse);
  Advent.print_part2 ~s:(string_of_int @@ part2 parse)
