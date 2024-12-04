let transform input =
  List.fold_right (List.map2 List.cons) input
  @@ List.init (List.length (List.hd input)) (fun _ -> [])

let diagonals input =
  let diagonal g get_elem =
    List.init (List.length g)
    @@ fun i -> List.init (List.length g - i) @@ fun j -> get_elem g (i + j) j
  in
  let bottom_left g = diagonal g (fun g i j -> List.nth (List.nth g j) i) in
  let top_right g = diagonal g (fun g i j -> List.nth (List.nth g i) j) in
  let flipped_grid = List.map List.rev input in
  bottom_left input
  @ List.tl (top_right input)
  @ bottom_left flipped_grid
  @ List.tl (top_right flipped_grid)

let rec amount_contains lst sub =
  match lst with
  | [] -> 0
  | _ when String.starts_with (String.of_seq (List.to_seq lst)) ~prefix:sub ->
    1 + amount_contains (List.tl lst) sub
  | _ -> amount_contains (List.tl lst) sub

let part1 input =
  let want = [ "XMAS"; "SAMX" ] in
  let sum_line lst = List.fold_left (fun acc fd -> acc + amount_contains lst fd) 0 want in
  let sum_grid g = List.fold_left (fun acc lst -> acc + sum_line lst) 0 g in
  List.fold_left
    (fun acc g -> acc + sum_grid g)
    0
    [ input; transform input; diagonals input ]

let blocks input =
  let get_at row = List.nth (List.nth input row) in
  List.concat
  @@ List.init (List.length input - 2)
  @@ fun i ->
  List.init (List.length input - 2)
  @@ fun j ->
  [ get_at i j
  ; get_at i (j + 2)
  ; get_at (i + 1) (j + 1)
  ; get_at (i + 2) j
  ; get_at (i + 2) (j + 2)
  ]

let part2 input =
  let check_block lst =
    let str = String.of_seq (List.to_seq lst) in
    if str = "MSAMS" || str = "SSAMM" || str = "MMASS" || str = "SMASM" then 1 else 0
  in
  List.fold_left (fun acc block -> acc + check_block block) 0 (blocks input)

let _ =
  let parse = List.map Base.String.to_list @@ Advent.read_lines_day ~d:4 ~i:Input in
  Advent.print_part1 ~s:(string_of_int @@ part1 parse);
  Advent.print_part2 ~s:(string_of_int @@ part2 parse)
