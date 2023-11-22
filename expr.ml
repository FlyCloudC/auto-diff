type tag = string

type exp =
  | Cst of float
  | Ap1 of Prim.fn * exp
  | Apn of Prim.op * exp list
  | Let of tag * exp * exp
  | Var of tag

type 'a env = (tag * 'a) list

exception Undefine of tag

let look env t =
  match List.assoc_opt t env with
  | Some v -> v
  | None -> raise (Undefine t)
;;

module Help = struct
  let ( + ) e1 e2 =
    match e1, e2 with
    | Apn (Add, es1), Apn (Add, es2) -> Apn (Add, es1 @ es2)
    | Apn (Add, es1), _ -> Apn (Add, es1 @ [ e2 ])
    | _, Apn (Add, es2) -> Apn (Add, e1 :: es2)
    | _ -> Apn (Add, [ e1; e2 ])
  ;;

  let ( * ) e1 e2 =
    match e1, e2 with
    | Apn (Mut, es1), Apn (Mut, es2) -> Apn (Mut, es1 @ es2)
    | Apn (Mut, es1), _ -> Apn (Mut, es1 @ [ e2 ])
    | _, Apn (Mut, es2) -> Apn (Mut, e1 :: es2)
    | _ -> Apn (Mut, [ e1; e2 ])
  ;;

  let ( ~- ) e = Ap1 (Neg, e)
  let ( - ) e1 e2 = e1 + -e2
  let ( / ) e1 e2 = e1 * Ap1 (Rev, e2)
  let const x = Cst x
  let ( = ) v e1 = v, e1
  let ( => ) (v, e1) e2 = Let (v, e1, e2)
  let var v = Var v
  let sin x = Ap1 (Sin, x)
  let cos x = Ap1 (Cos, x)
  let exp x = Ap1 (Exp, x)
  let ln x = Ap1 (Ln, x)
  let ( ** ) e1 e2 = exp (ln e1 * e2)
end
