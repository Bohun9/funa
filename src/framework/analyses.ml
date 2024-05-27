open Api
open Lang.Ast
open Utils

let registery = Hashtbl.create 7

module RegisterAnalysis (P : sig
  val name : string

  module A : MonotoneInstance
end) =
struct
  open P

  let _ = Hashtbl.add registery name (module A : MonotoneInstance)
end

module CallAnalysisMonotoneInstance : MonotoneInstance = struct
  type t = FuncSet.t

  let to_string = funcset_to_string
  let to_funcset x = x
  let bot _ = FuncSet.empty
  let join = FuncSet.union
  let less_or_equal = FuncSet.subset

  let constraints (cfg : cfg) (e : expr) (st : t state) (ci : cxt_info) :
      t changes =
    let cache_cfg = cache cfg in
    let cache = cache st in
    let env = env st in
    match e.data with
    | EApp (e1, e2) ->
        (Lab (e.label, ci.cxt), cache e1.label ci.cxt)
        :: (Lab (e.label, ci.cxt), cache e2.label ci.cxt)
        :: FuncSet.fold
             (fun (Func (l0, x, e0, cxt0)) acc ->
               ( Lab (e.label, ci.cxt),
                 FuncSet.singleton (Func (l0, x, e0, cxt0)) )
               :: acc)
             (cache_cfg e1.label ci.cxt)
             []
    | EBinop (_, e1, e2) ->
        [
          (Lab (e.label, ci.cxt), cache e1.label ci.cxt);
          (Lab (e.label, ci.cxt), cache e2.label ci.cxt);
        ]
    | EUnop (_, e1) | EIf (e1, _, _) ->
        [ (Lab (e.label, ci.cxt), cache e1.label ci.cxt) ]
    | _ -> []

  let gen_flow_constraints = true
  let gen_var_constraints = false
end

module _ = RegisterAnalysis (struct
  let name = "call_analysis"

  module A = CallAnalysisMonotoneInstance
end)
