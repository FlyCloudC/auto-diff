open Prim
open Expr

type loc =
  | Cst of float
  | Reg of int
  | Read of Expr.tag

type inst =
  | Ap1 of Prim.fn * loc
  | Ap2 of Prim.op * loc * loc

type program = loc * inst array

let compile (parms : tag list) e =
  let inst_list : inst list ref = ref [] in
  let allo : inst -> loc =
    let reg_id = ref 0 in
    fun i ->
      let reg = Reg !reg_id in
      incr reg_id;
      inst_list := i :: !inst_list;
      reg
  in
  let rec eval (renv : loc env) (e : exp) : loc =
    match e with
    | Cst x -> Cst x
    | Ap1 (fn, e') ->
      let v = eval renv e' in
      allo (Ap1 (fn, v))
    | Apn (op, es) ->
      let rec combine v1 vs =
        match vs with
        | [] -> v1
        | v2 :: vs' ->
          let v12 = allo (Ap2 (op, v1, v2)) in
          combine v12 vs'
      in
      (match List.map (eval renv) es with
       | [] -> failwith "combine"
       | v1 :: vs -> combine v1 vs)
    | Let (t, e1, e2) ->
      let v1 = eval renv e1 in
      let new_renv = (t, v1) :: renv in
      eval new_renv e2
    | Var t ->
      (match List.assoc_opt t renv with
       | Some v -> v
       | None -> raise (Undefine t))
  in
  let final = eval (List.map (fun t -> t, Read t) parms) e in
  final, Array.of_list (List.rev !inst_list)
;;

let look va env = function
  | Cst v -> v
  | Reg id -> va.(id)
  | Read t -> Expr.look env t
;;

let run env pg =
  let final, insts = pg in
  let len = Array.length insts in
  let va = Array.create_float len in
  let f = look va env in
  for i = 0 to len - 1 do
    va.(i)
    <- (match insts.(i) with
        | Ap1 (fn, v) -> eval_fn fn (f v)
        | Ap2 (op, v1, v2) -> eval_op op (f v1) (f v2))
  done;
  f final, va
;;
