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

val eval_fn : fn -> float -> float
val diff_fn : fn -> float -> float
val eval_op : op -> float -> float -> float
