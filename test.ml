open Expr
open Inst
open Diff

let exp1 = Help.(ln (var "x1") + (var "x1" * var "x2") - sin (var "x2"))
let env1 = [ "x1", 2.; "x2", 5. ]
let pg1 = compile (List.map fst env1) exp1
let res1 = run env1 pg1
let d_x1 = run_diff env1 pg1 "x1"
let d_x2 = run_diff env1 pg1 "x2"
let d_all, da = run_diff_all env1 pg1
