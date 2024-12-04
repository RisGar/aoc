(** Helper functions like combinators that should be opened *)
module Helpers = Helpers

(** Whether example or puzzle input should be read *)
type input = Example | Input

(** Example or puzzle input returned as a [string list] of lines  *)
val read_lines_day : d:int -> i:input -> string list

(** Example or puzzle input returned as a whole [string]  *)
val read_file_day : d:int -> i:input -> string

(** Print the result of part 1 *)
val print_part1 : s:string -> unit

(** Print the result of part 2 *)
val print_part2 : s:string -> unit
