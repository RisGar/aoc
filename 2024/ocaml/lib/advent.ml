type input =
  | Example
  | Input

let get_end_str e =
  match e with
  | Example -> "example"
  | Input -> "input"
;;

let format_int_two_digits n = Printf.sprintf "%02d" n

let read_lines_day ~d:day ~i:input =
  In_channel.with_open_text
    ("inputs/" ^ format_int_two_digits day ^ "-" ^ get_end_str input ^ ".txt")
    In_channel.input_lines
;;

let read_file_day ~d:day ~i:input =
  In_channel.with_open_text
    ("inputs/" ^ format_int_two_digits day ^ "-" ^ get_end_str input ^ ".txt")
    In_channel.input_all
;;

let print_part1 ~s:str = print_endline ("Part 1: " ^ str)
let print_part2 ~s:str = print_endline ("Part 2: " ^ str)
