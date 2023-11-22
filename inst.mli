type loc =
  | Cst of float
  | Reg of int
  | Read of Expr.tag

type inst =
  | Ap1 of Prim.fn * loc
  | Ap2 of Prim.op * loc * loc

type program = loc * inst array

val compile : Expr.tag list -> Expr.exp -> program
val look : float array -> float Expr.env -> loc -> float
val run : float Expr.env -> program -> float * float array
