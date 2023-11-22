type fn =
  | Neg
  | Rev
  | Sin
  | Cos
  | Exp
  | Ln

type op =
  | Add
  | Mut

let eval_fn = function
  | Neg -> fun x -> -.x
  | Rev -> fun x -> 1. /. x
  | Sin -> sin
  | Cos -> cos
  | Exp -> exp
  | Ln -> log
;;

let diff_fn = function
  | Neg -> fun _ -> -1.
  | Rev -> fun x -> -1. /. (x *. x)
  | Sin -> cos
  | Cos -> fun x -> -.sin x
  | Exp -> exp
  | Ln -> fun x -> 1. /. x
;;

let eval_op = function
  | Add -> ( +. )
  | Mut -> ( *. )
;;
