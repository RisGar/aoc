let rec zip_with_next = function
  | [] | [ _ ] -> []
  | x :: (y :: _ as tl) -> (int_of_string x, int_of_string y) :: zip_with_next tl
;;

let parse =
  List.map (fun s -> String.split_on_char ' ' s) (Advent.read_lines_day ~d:2 ~i:Input)
;;

let get_diff ~s ~x ~y =
  let is_increasing s =
    if let x, y = List.hd s in
       x < y
    then true
    else false
  in
  match is_increasing s with
  | true -> y - x
  | false -> x - y
;;

let is_valid_diff ~d:diff = diff >= 1 && diff <= 3

let part1 input =
  List.length
  @@ List.filter
       (fun s ->
         let s' = zip_with_next s in
         List.for_all (fun (x, y) -> is_valid_diff ~d:(get_diff ~s:s' ~x ~y)) s')
       input
;;

let part2 input =
  List.length
  @@ List.filter
       (fun s ->
         let s' = zip_with_next s in
         if List.for_all
              (fun (x, y) ->
                let diff = get_diff ~s:s' ~x ~y in
                diff >= 1 && diff <= 3)
              s'
            = true
         then true
         else
           Base.List.existsi
             ~f:(fun i _ ->
               let s'' = zip_with_next @@ List.filteri (fun i' _ -> i <> i') @@ s in
               let res =
                 List.for_all
                   (fun (x, y) ->
                     let diff = get_diff ~s:s'' ~x ~y in
                     diff >= 1 && diff <= 3)
                   s''
               in
               res)
             s)
       input
;;

let _ =
  Advent.print_part1 ~s:(string_of_int @@ part1 parse);
  Advent.print_part2 ~s:(string_of_int @@ part2 parse)
;;
