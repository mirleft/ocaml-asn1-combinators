
type 'a endo = 'a -> 'a

let id x = x

let const x _ = x

let comp f g x = f (g x)

