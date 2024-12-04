
(** Function composition in f# syntax *)
val ( << ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

(** Reverse function composition in f# syntax *)
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** Uncurry a function to accept a tuple *)
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
