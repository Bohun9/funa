open Lang.Ast
open Api
module StringMap = Map.Make (String)

let rec _gen_contexts (prefix : context) (labs : label list) (k : int) :
    context list =
  if List.length prefix > k then []
  else
    List.fold_left
      (fun acc l -> acc @ _gen_contexts (l :: prefix) labs k)
      [ prefix ] labs

let gen_contexts (labs : label list) (k : int) = _gen_contexts [] labs k

let find_dft (dft : 'b) (hm : ('a, 'b) Hashtbl.t) (x : 'a) : 'b =
  match Hashtbl.find_opt hm x with
  | Some y -> y
  | None ->
      Hashtbl.replace hm x dft;
      Hashtbl.find hm x

let cache (dft : 'a) (st : 'a state) (l : label) (cxt : context) : 'a =
  find_dft dft st (Lab (l, cxt))

let env (dft : 'a) (st : 'a state) (x : var) (cxt : context) : 'a =
  find_dft dft st (Var (x, cxt))

let free_vars_of_fun (Func (_, x, e0, _) : func) : VarSet.t =
  VarSet.remove x (free_vars_of_expr e0)
