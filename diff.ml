open Expr
open Inst

let run_diff env pg var =
  let final, insts = pg in
  let len = Array.length insts in
  let _, va = run env pg in
  let da = Array.create_float (Array.length va) in
  let f = look va env in
  let f' = function
    | Cst v -> 0.
    | Reg id -> da.(id)
    | Read t -> if t = var then 1. else 0.
  in
  for i = 0 to len - 1 do
    da.(i)
    <- (match insts.(i) with
        | Ap1 (fn, v) -> f' v *. Prim.diff_fn fn (f v)
        | Ap2 (Add, v1, v2) -> f' v1 +. f' v2
        | Ap2 (Mut, v1, v2) -> (f' v1 *. f v2) +. (f' v2 *. f v1))
  done;
  f' final, da
;;

let run_diff_all env pg =
  let final, insts = pg in
  match final with
  | Cst _ -> List.map (fun (t, _) -> t, 0.) env, [||]
  | Read t -> List.map (fun (t', _) -> t', if t = t' then 1. else 0.) env, [||]
  | Reg final_id ->
    let _, va = run env pg in
    let da = Array.make (final_id + 1) 0. in
    let denv = List.map (fun (t', _) -> t', ref 0.) env in
    let ( += ) v x =
      match v with
      | Cst _ -> ()
      | Reg id -> da.(id) <- da.(id) +. x
      | Read t ->
        let rx = List.assoc t denv in
        rx := !rx +. x
    in
    let f = look va env in
    da.(final_id) <- 1.;
    for i = final_id downto 0 do
      match insts.(i) with
      | Ap1 (fn, v) -> v += (da.(i) *. Prim.diff_fn fn (f v))
      | Ap2 (Add, v1, v2) ->
        v1 += da.(i);
        v2 += da.(i)
      | Ap2 (Mut, v1, v2) ->
        v1 += (da.(i) *. f v2);
        v2 += (da.(i) *. f v1)
    done;
    List.map (fun (t, rx) -> t, !rx) denv, da
;;
