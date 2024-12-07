type tile = Empty | Obstacle | Guard
type direction = Up | Down | Left | Right

let direction_to_int = function
  | Up -> -1, 0
  | Down -> 1, 0
  | Left -> 0, -1
  | Right -> 0, 1

let turn_direction = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down

module PosDirSet = Set.Make (struct
    type t = (int * int) * direction

    let compare = compare
  end)

let parse =
  let input = Advent.read_lines_day ~d:6 ~i:Input in
  List.flatten
  @@ List.mapi (fun i line ->
    List.mapi (fun j char ->
      let tile_type =
        match char with
        | '#' -> Obstacle
        | '^' -> Guard
        | _ -> Empty
      in
      (i, j), tile_type)
    @@ List.of_seq
    @@ String.to_seq line)
  @@ input

let add_tuple (m, n) (x, y) = m + x, n + y

let rec continue_walk tbl dir visited already_visited =
  let guard_pos =
    match Hashtbl.to_seq tbl |> Seq.find (fun (_, tile_type) -> tile_type = Guard) with
    | Some ((x, y), _) -> x, y
    | None -> failwith "Guard not found"
  in
  let new_pos = add_tuple guard_pos (direction_to_int dir) in
  match Hashtbl.find_opt tbl new_pos with
  | Some Obstacle -> continue_walk tbl (turn_direction dir) visited already_visited
  | Some _ ->
    Hashtbl.replace tbl guard_pos Empty;
    Hashtbl.replace tbl new_pos Guard;
    (match List.find_opt (( = ) guard_pos) already_visited with
     | Some _ -> continue_walk tbl dir visited already_visited
     | None -> continue_walk tbl dir (visited + 1) (guard_pos :: already_visited))
  | None -> visited

let part1 list =
  let tbl = Hashtbl.create (List.length list) in
  let seq = List.to_seq list in
  Hashtbl.add_seq tbl seq;
  continue_walk tbl Up 1 []

let try_adding_obstacle map o_pos =
  let is_obstacle pos =
    if pos = o_pos
    then 2
    else (
      match Hashtbl.find_opt map pos with
      | Some Obstacle -> 2
      | Some _ -> 1
      | None -> 0)
  in
  let guard_pos =
    match Hashtbl.to_seq map |> Seq.find (fun (_, tile_type) -> tile_type = Guard) with
    | Some ((n, m), _) -> n, m
    | None -> failwith "Guard not found"
  in
  let rec continue_walk' pos dir already_visited =
    if PosDirSet.mem (pos, dir) already_visited
    then 1
    else (
      let new_pos = add_tuple pos (direction_to_int dir) in
      match is_obstacle new_pos with
      | 2 -> continue_walk' pos (turn_direction dir) already_visited
      | 1 -> continue_walk' new_pos dir (PosDirSet.add (pos, dir) already_visited)
      | 0 -> 0
      | _ -> failwith "impossible")
  in
  continue_walk' guard_pos Up PosDirSet.empty

let part2 list =
  let positions = Array.of_list list in
  let len_positions = Array.length positions in
  let map = Hashtbl.of_seq @@ List.to_seq list in
  let num_domains = Domain.recommended_domain_count () in
  let pool = Domainslib.Task.setup_pool ~num_domains () in
  let result =
    Domainslib.Task.run pool (fun _ ->
      Domainslib.Task.parallel_for_reduce
        pool
        ~chunk_size:(max 1 (len_positions / num_domains * 4))
        ~start:0
        ~finish:(len_positions - 1)
        ~body:(fun i ->
          match positions.(i) with
          | pos, Empty -> try_adding_obstacle map pos
          | _ -> 0)
        ( + )
        0)
  in
  Domainslib.Task.teardown_pool pool;
  result

let _ =
  Advent.print_part1 ~s:(string_of_int @@ part1 parse);
  Advent.print_part2 ~s:(string_of_int @@ part2 parse)
