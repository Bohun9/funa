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

module IntegerConstantPropagationMonotoneInstance = struct
  type t =
    | Bot (* definitely not an integer *)
    | Int of int (* exactly this integer *)
    | Top (* at least 2 different integers *)

  let to_string (z : t) : string =
    match z with Bot -> "⊥" | Int i -> string_of_int i | Top -> "⊤"

  let to_funcset _ = failwith "internal error"
  let bot _ = Bot

  let join z1 z2 =
    match (z1, z2) with
    | Int n1, Int n2 when n1 = n2 -> Int n1
    | Int _, Int _ | Top, _ | _, Top -> Top
    | Bot, Int _ -> z2
    | Int _, Bot -> z1
    | Bot, Bot -> Bot

  let less_or_equal z1 z2 =
    match (z1, z2) with
    | _, Top | Bot, _ -> true
    | Int n1, Int n2 when n1 = n2 -> true
    | _, _ -> false

  let constraints (cfg : cfg) (e : expr) (st : t state) (ci : cxt_info) :
      t changes =
    match e.data with
    | EInt i -> [ (Lab (e.label, ci.cxt), Int i) ]
    | EUnop (_, _) -> []
    | EBinop (op, e1, e2) ->
        [
          ( Lab (e.label, ci.cxt),
            match op with
            | BINOP_Add -> (
                match (cache st e1.label ci.cxt, cache st e2.label ci.cxt) with
                | Bot, _ | _, Bot -> Bot
                | Int n1, Int n2 -> Int (n1 + n2)
                | _, _ -> Top)
            | BINOP_Mul -> (
                match (cache st e1.label ci.cxt, cache st e2.label ci.cxt) with
                | Bot, _ | _, Bot -> Bot
                | Int n1, Int n2 -> Int (n1 * n2)
                | _, _ -> Top) );
        ]
    | _ -> []

  let gen_flow_constraints = true
  let gen_var_constraints = true
end

module _ = RegisterAnalysis (struct
  let name = "constant_propagation"

  module A = IntegerConstantPropagationMonotoneInstance
end)
