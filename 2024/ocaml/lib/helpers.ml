let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let uncurry f (x, y) = f x y
