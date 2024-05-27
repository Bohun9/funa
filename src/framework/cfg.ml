open Lang.Ast
open Api
open Utils

module ControlFlowMonotoneInstance : MonotoneInstance = struct
  type t = FuncSet.t

  let to_string = funcset_to_string
  let to_funcset x = x
  let bot _ = FuncSet.empty
  let join = FuncSet.union
  let less_or_equal = FuncSet.subset

  let constraints (_ : cfg) (e : expr) (st : t state) (ci : cxt_info) :
      t changes =
    let cache = cache st in
    let env = env st in
    match e.data with
    | ELam (x, b) ->
        [
          ( Lab (e.label, ci.cxt),
            FuncSet.singleton (Func (e.label, x, b, ci.cxt)) );
        ]
    | ERec (s, x, b) ->
        [
          ( Lab (e.label, ci.cxt),
            FuncSet.singleton (Func (e.label, x, b, ci.cxt)) );
          (Var (s, ci.cxt), FuncSet.singleton (Func (e.label, x, b, ci.cxt)));
        ]
    | EApp (e1, e2) ->
        FuncSet.fold
          (fun (Func (l0, x, e0, cxt0) as f) acc ->
            (Var (x, ci.push e.label), cache e2.label ci.cxt)
            :: (Lab (e.label, ci.cxt), cache e0.label (ci.push e.label))
            :: acc
            @ VarSet.fold
                (fun y acc -> (Var (y, ci.push e.label), env y cxt0) :: acc)
                (free_vars_of_fun f) [])
          (cache e1.label ci.cxt) []
    | _ -> []

  let gen_flow_constraints = FCAll
  let gen_var_constraints = true
end
