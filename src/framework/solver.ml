open Lang.Ast
open Api
open Utils

let convert_results_to_string (result : 'a Api.state) (to_string : 'a -> string)
    : string Api.state =
  result |> Hashtbl.to_seq
  |> Seq.map (fun (d, y) -> (d, to_string y))
  |> Hashtbl.of_seq

module type SolverParams = sig
  val k_cfa : int
  val cfg : cfg option
end

module Solver (MI : MonotoneInstance) (Params : SolverParams) = struct
  module Worker (Prog : sig
    val program : expr
  end) =
  struct
    open Prog

    let vars = vars_of_expr program
    let labels = labels program
    let app_labels = app_labels program
    let contexts = gen_contexts app_labels Params.k_cfa

    let fill_state (s : 'a state) (bot : 'a) =
      List.iter
        (fun cxt ->
          List.iter (fun x -> Hashtbl.add s (Var (x, cxt)) bot) vars;
          List.iter (fun l -> Hashtbl.add s (Lab (l, cxt)) bot) labels)
        contexts

    let cfg =
      match Params.cfg with
      | Some cfg -> cfg
      | None ->
          let dummy_cfg = Hashtbl.create 127 in
          let _ = fill_state dummy_cfg FuncSet.empty in
          dummy_cfg

    let bot = MI.bot program

    let cxt_push cxt l =
      let cxt = cxt @ [ l ] in
      if List.length cxt <= Params.k_cfa then cxt else List.tl cxt

    let result : MI.t state =
      let r = Hashtbl.create 127 in
      fill_state r (MI.bot program);
      r

    let cache_cfg = cache cfg
    let cache = cache result
    let env = env result

    let explicit_changes (e : expr) : MI.t changes =
      List.fold_left
        (fun acc cxt ->
          acc @ MI.constraints cfg e result { cxt; push = cxt_push cxt })
        [] contexts

    let implicit_var_changes (e : expr) : MI.t changes =
      List.fold_left
        (fun acc cxt ->
          acc
          @
          match e.data with
          | EInt _ | EBool _ -> []
          | EVar x -> [ (Lab (e.label, cxt), env x cxt) ]
          | EIf (_, _, _) -> []
          | ELet (x, e1, _) -> [ (Var (x, cxt), cache e1.label cxt) ]
          | EApp (e1, e2) ->
              FuncSet.fold
                (fun (Func (_, x, _, _)) acc ->
                  (Var (x, cxt_push cxt e.label), cache e2.label cxt) :: acc)
                (cache_cfg e1.label cxt) []
          | ELam (_, _) -> []
          | ERec (_, _, _) -> []
          | EUnop (_, _) -> []
          | EBinop (_, _, _) -> [])
        [] contexts

    let implicit_flow_changes (e : expr) : MI.t changes =
      List.fold_left
        (fun acc cxt ->
          acc
          @
          match e.data with
          | EInt _ | EBool _ | EVar _ | ELam (_, _) | ERec (_, _, _) -> []
          | EIf (_, e2, e3) ->
              [
                (Lab (e.label, cxt), cache e2.label cxt);
                (Lab (e.label, cxt), cache e3.label cxt);
              ]
          | ELet (_, _, e2) -> [ (Lab (e.label, cxt), cache e2.label cxt) ]
          | EApp (e1, _) ->
              FuncSet.fold
                (fun (Func (_, _, e0, _)) acc ->
                  ( Lab (e.label, cxt_push cxt e.label),
                    cache e0.label (cxt_push cxt e.label) )
                  :: acc)
                (cache_cfg e1.label cxt) []
          | EUnop (_, _) | EBinop (_, _, _) -> [])
        [] contexts

    let implicit_app_changes (e : expr) : MI.t changes =
      match e.data with
      | EApp (e1, e2) ->
          List.fold_left
            (fun acc cxt ->
              acc
              @ FuncSet.fold
                  (fun (Func (_, _, e0, ctx0) as f) acc ->
                    VarSet.fold
                      (fun y acc ->
                        (Var (y, ctx0), env y (cxt_push cxt e.label)) :: acc)
                      (free_vars_of_fun f) []
                    @ acc)
                  (cache_cfg e1.label cxt) [])
            [] contexts
      | _ -> []

    let rec collect_changes (e : expr) : MI.t changes =
      let changes_explicit = explicit_changes e in
      let changes_implicit_var =
        if MI.gen_var_constraints then implicit_var_changes e else []
      in
      let changes_implicit_flow =
        if MI.gen_flow_constraints then implicit_flow_changes e else []
      in
      let changes_implicit = changes_implicit_var @ changes_implicit_flow in
      let changes_implicit_app = implicit_app_changes e in
      let changes_down =
        match e.data with
        | EInt _ | EBool _ | EVar _ -> []
        | EIf (e1, e2, e3) ->
            collect_changes e1 @ collect_changes e2 @ collect_changes e3
        | ELet (_, e1, e2) | EApp (e1, e2) | EBinop (_, e1, e2) ->
            collect_changes e1 @ collect_changes e2
        | ELam (_, e1) | ERec (_, _, e1) | EUnop (_, e1) -> collect_changes e1
      in
      changes_explicit @ changes_implicit @ changes_down @ changes_implicit_app

    let apply_changes (changes : MI.t changes) : bool =
      List.fold_left
        (fun ch (x, y) ->
          let old = Hashtbl.find result x in
          Hashtbl.replace result x (MI.join y old);
          ch || not (MI.less_or_equal y old))
        false changes

    let step () : bool =
      let changes = collect_changes program in
      apply_changes changes

    let rec run () = if step () then run () else ()
  end

  let analyse (program : expr) : MI.t state =
    let module Instance = Worker (struct
      let program = program
    end) in
    Instance.run ();
    Instance.result
end
