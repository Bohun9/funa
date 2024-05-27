open Api
open Lang.Ast

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
  type t = LabelSet.t

  let to_string = labelset_to_string
  let to_funcset _ = failwith "internal error"
  let bot _ = LabelSet.empty
  let join = LabelSet.union
  let less_or_equal = LabelSet.subset

  let constraints (cache_cfg : cache_cfg) (e : expr)
      ((cache, _) : t cache * t env) (ci : cxt_info) : t changes =
    match e.data with
    | EApp (e1, e2) ->
        ( Lab (e.label, ci.cxt),
          cache e1.label ci.cxt,
          [ Lab (e1.label, ci.cxt) ] )
        :: ( Lab (e.label, ci.cxt),
             cache e2.label ci.cxt,
             [ Lab (e2.label, ci.cxt) ] )
        :: FuncSet.fold
             (fun (Func (l0, _, _, _)) acc ->
               (Lab (e.label, ci.cxt), LabelSet.singleton l0, []) :: acc)
             (cache_cfg e1.label ci.cxt)
             []
    | EBinop (_, e1, e2) ->
        [
          ( Lab (e.label, ci.cxt),
            cache e1.label ci.cxt,
            [ Lab (e1.label, ci.cxt) ] );
          ( Lab (e.label, ci.cxt),
            cache e2.label ci.cxt,
            [ Lab (e2.label, ci.cxt) ] );
        ]
    | EUnop (_, e1) | EIf (e1, _, _) ->
        [
          ( Lab (e.label, ci.cxt),
            cache e1.label ci.cxt,
            [ Lab (e1.label, ci.cxt) ] );
        ]
    | _ -> []

  let gen_flow_constraints = FCAll
  let gen_var_constraints = false
end

module _ = RegisterAnalysis (struct
  let name = "call_analysis"

  module A = CallAnalysisMonotoneInstance
end)

module ConstantPropagationMonotoneInstance = struct
  type tz =
    | ZBot (* definitely not an integer *)
    | ZInt of int (* exactly this integer *)
    | ZTop (* at least 2 different integers *)

  module BoolSet = Set.Make (Bool)

  type tb = BoolSet.t
  type t = tz * tb

  let to_string ((z, b) : t) : string =
    let z_string =
      match z with ZBot -> "⊥" | ZInt i -> string_of_int i | ZTop -> "⊤"
    in
    let b_string =
      BoolSet.to_seq b
      |> Seq.map (fun b -> match b with true -> "true" | false -> "false")
      |> List.of_seq |> String.concat "," |> Printf.sprintf "{%s}"
    in
    Printf.sprintf "Z:%s B:%s" z_string b_string

  let zbot = ZBot
  let bbot = BoolSet.empty
  let btop = [ true; false ] |> List.to_seq |> BoolSet.of_seq
  let to_funcset _ = failwith "internal error"
  let bot _ = (zbot, bbot)

  let zjoin z1 z2 =
    match (z1, z2) with
    | ZInt n1, ZInt n2 when n1 = n2 -> ZInt n1
    | ZInt _, ZInt _ | ZTop, _ | _, ZTop -> ZTop
    | ZBot, ZInt _ -> z2
    | ZInt _, ZBot -> z1
    | ZBot, ZBot -> ZBot

  let bjoin = BoolSet.union
  let join (z1, b1) (z2, b2) = (zjoin z1 z2, bjoin b1 b2)

  let less_or_equal (z1, b1) (z2, b2) =
    (match (z1, z2) with
    | _, ZTop | ZBot, _ -> true
    | ZInt n1, ZInt n2 when n1 = n2 -> true
    | _, _ -> false)
    && BoolSet.subset b1 b2

  let constraints (_ : cache_cfg) (e : expr) ((cache, _) : t cache * t env)
      (ci : cxt_info) : t changes =
    match e.data with
    | EInt i -> [ (Lab (e.label, ci.cxt), (ZInt i, bbot), []) ]
    | EUnop (op, e1) -> (
        match op with
        | UNOP_Not ->
            let _, b1 = cache e1.label ci.cxt in
            let b1 =
              b1 |> BoolSet.to_seq |> Seq.map (fun b -> not b) |> BoolSet.of_seq
            in
            [ (Lab (e.label, ci.cxt), (zbot, b1), [ Lab (e1.label, ci.cxt) ]) ])
    | EBinop (op, e1, e2) ->
        let z1, _ = cache e1.label ci.cxt in
        let z2, _ = cache e2.label ci.cxt in
        let z =
          match (z1, z2) with
          | ZBot, _ | _, ZBot -> ZBot
          | ZInt n1, ZInt n2 -> (
              match op with
              | BINOP_Add -> ZInt (n1 + n2)
              | BINOP_Sub -> ZInt (n1 - n2)
              | BINOP_Mul -> ZInt (n1 * n2))
          | _, _ -> ZTop
        in
        [
          ( Lab (e.label, ci.cxt),
            (z, bbot),
            [ Lab (e1.label, ci.cxt); Lab (e2.label, ci.cxt) ] );
        ]
    | ERelop (op, e1, e2) ->
        let z1, _ = cache e1.label ci.cxt in
        let z2, _ = cache e2.label ci.cxt in
        let b =
          match (z1, z2) with
          | ZBot, _ | _, ZBot -> bbot
          | ZInt n1, ZInt n2 -> (
              match op with
              | RELOP_Gt -> BoolSet.singleton (n1 > n2)
              | RELOP_Lt -> BoolSet.singleton (n1 < n2)
              | RELOP_Ge -> BoolSet.singleton (n1 >= n2)
              | RELOP_Le -> BoolSet.singleton (n1 <= n2))
          | _, _ -> btop
        in
        [
          ( Lab (e.label, ci.cxt),
            (zbot, b),
            [ Lab (e1.label, ci.cxt); Lab (e2.label, ci.cxt) ] );
        ]
    (* Custom handle of the if *)
    | EIf (e1, e2, e3) -> (
        let _, b1 = cache e1.label ci.cxt in
        let c2 =
          ( Lab (e.label, ci.cxt),
            cache e2.label ci.cxt,
            [ Lab (e1.label, ci.cxt); Lab (e2.label, ci.cxt) ] )
        in
        let c3 =
          ( Lab (e.label, ci.cxt),
            cache e3.label ci.cxt,
            [ Lab (e1.label, ci.cxt); Lab (e3.label, ci.cxt) ] )
        in
        match (BoolSet.mem true b1, BoolSet.mem false b1) with
        | true, true -> [ c2; c3 ]
        | true, false -> [ c2 ]
        | false, true -> [ c3 ]
        | false, false -> [])
    | _ -> []

  let gen_flow_constraints = FCAllWithoutIf
  let gen_var_constraints = true
end

module _ = RegisterAnalysis (struct
  let name = "constant_propagation"

  module A = ConstantPropagationMonotoneInstance
end)
