type tag = string

type exp =
  | Cst of float
  | Ap1 of Prim.fn * exp
  | Apn of Prim.op * exp list
  | Let of tag * exp * exp
  | Var of tag

type 'a env = (tag * 'a) list

exception Undefine of tag

val look : 'a env -> tag -> 'a

module Help : sig
  val ( + ) : exp -> exp -> exp
  val ( * ) : exp -> exp -> exp
  val ( ~- ) : exp -> exp
  val ( - ) : exp -> exp -> exp
  val ( / ) : exp -> exp -> exp
  val const : float -> exp
  val ( = ) : tag -> exp -> tag * exp
  val ( => ) : tag * exp -> exp -> exp
  val var : tag -> exp
  val sin : exp -> exp
  val cos : exp -> exp
  val exp : exp -> exp
  val ln : exp -> exp
  val ( ** ) : exp -> exp -> exp
end
