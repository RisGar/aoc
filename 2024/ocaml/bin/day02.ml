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
    then 1
    else 0
  in
  match is_increasing s with
  | 1 -> y - x
  | 0 -> x - y
  | _ -> 8
;;

let part1 input =
  string_of_int
  @@ List.length
  @@ List.filter
       (fun s ->
         let s' = zip_with_next s in
         List.for_all
           (fun (x, y) ->
             let diff = get_diff ~s:s' ~x ~y in
             diff >= 1 && diff <= 3)
           s')
       input
;;

let part2 input =
  string_of_int
  @@ List.length
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
           List.exists
             (( = ) true)
             (List.mapi
                (fun i _ ->
                  let s'' = zip_with_next @@ List.filteri (fun i' _ -> i <> i') @@ s in
                  let res =
                    List.for_all
                      (fun (x, y) ->
                        let diff = get_diff ~s:s'' ~x ~y in
                        diff >= 1 && diff <= 3)
                      s''
                  in
                  res)
                s))
       input
;;

let _ =
  Advent.print_part1 ~s:(part1 parse);
  Advent.print_part2 ~s:(part2 parse)
;;
