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
    let lab_expr_mapping = Lang.Label.label_expr_mapping program

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
      (* fill_state r (MI.bot program); *)
      r

    let cache_cfg = cache FuncSet.empty cfg
    let cache = cache (MI.bot program) result
    let env = env (MI.bot program) result

    type form = expr * context

    module FormSet = Set.Make (struct
      type t = form

      let compare = compare
    end)

    let todo = ref FormSet.empty
    let visited = ref FormSet.empty

    let explicit_changes (e : expr) (cxt : context) : MI.t changes =
      MI.constraints cache_cfg e (cache, env) { cxt; push = cxt_push cxt }

    let implicit_var_changes (e : expr) (cxt : context) : MI.t changes =
      match e.data with
      | EInt _ | EBool _ -> []
      | EVar x -> [ (Lab (e.label, cxt), env x cxt, [ Var (x, cxt) ]) ]
      | EIf (_, _, _) -> []
      | ELet (x, e1, _) ->
          [ (Var (x, cxt), cache e1.label cxt, [ Lab (e1.label, cxt) ]) ]
      | EApp (e1, e2) ->
          FuncSet.fold
            (fun (Func (_, x, _, _)) acc ->
              ( Var (x, cxt_push cxt e.label),
                cache e2.label cxt,
                [ Lab (e2.label, cxt) ] )
              :: acc)
            (cache_cfg e1.label cxt) []
      | ELam (_, _) -> []
      | ERec (_, _, _) -> []
      | EUnop (_, _) -> []
      | EBinop (_, _, _) | ERelop (_, _, _) -> []

    let implicit_flow_changes (e : expr) (fcs : flow_constr_specifier)
        (cxt : context) : MI.t changes =
      match e.data with
      | EInt _ | EBool _ | EVar _ | ELam (_, _) | ERec (_, _, _) -> []
      | EIf (e1, e2, e3) ->
          (match fcs with
          | FCAll ->
              [
                (Lab (e.label, cxt), cache e2.label cxt, [ Lab (e2.label, cxt) ]);
                (Lab (e.label, cxt), cache e3.label cxt, [ Lab (e3.label, cxt) ]);
              ]
          | _ -> [])
          @ [ (Lab (e.label, cxt), bot, [ Lab (e1.label, cxt) ]) ]
      | ELet (_, _, e2) ->
          [ (Lab (e.label, cxt), cache e2.label cxt, [ Lab (e2.label, cxt) ]) ]
      | EApp (e1, _) ->
          FuncSet.fold
            (fun (Func (_, _, e0, _)) acc ->
              ( Lab (e.label, cxt),
                cache e0.label (cxt_push cxt e.label),
                [ Lab (e0.label, cxt_push cxt e.label) ] )
              :: acc)
            (cache_cfg e1.label cxt)
            [ (Lab (e.label, cxt), bot, [ Lab (e1.label, cxt) ]) ]
          (* Dummy constraint to analyse left operand ^^^^^ *)
          (* If constraints are adjusted to "worklist style" there will be no need for it *)
      | EUnop (_, e1) -> [ (Lab (e.label, cxt), bot, [ Lab (e1.label, cxt) ]) ]
      | EBinop (_, e1, e2) | ERelop (_, e1, e2) ->
          [
            (Lab (e.label, cxt), bot, [ Lab (e1.label, cxt) ]);
            (Lab (e.label, cxt), bot, [ Lab (e2.label, cxt) ]);
          ]

    let implicit_app_changes (e : expr) (cxt : context) : MI.t changes =
      match e.data with
      | EApp (e1, _) ->
          FuncSet.fold
            (fun (Func (_, _, _, cxt0) as f) acc ->
              VarSet.fold
                (fun y acc ->
                  (Var (y, cxt_push cxt e.label), env y cxt0, [ Var (y, cxt0) ])
                  :: acc)
                (free_vars_of_fun f) []
              @ acc)
            (cache_cfg e1.label cxt) []
      | _ -> []

    let rec collect_changes (e : expr) (cxt : context) : MI.t changes =
      if FormSet.mem (e, cxt) !visited then []
      else (
        visited := FormSet.add (e, cxt) !visited;
        let changes_explicit = explicit_changes e cxt in
        let changes_implicit_var =
          if MI.gen_var_constraints then implicit_var_changes e cxt else []
        in
        let changes_implicit_flow =
          if match MI.gen_flow_constraints with FCNone -> false | _ -> true
          then implicit_flow_changes e MI.gen_flow_constraints cxt
          else []
        in
        let changes_implicit = changes_implicit_var @ changes_implicit_flow in
        let changes_implicit_app = implicit_app_changes e cxt in
        changes_explicit @ changes_implicit @ changes_implicit_app)

    let apply_changes (changes : MI.t changes) : bool =
      (* let _ = print_endline "CHANGES:" in *)
      (* List.iter *)
      (*   (fun (d, y, deps) -> *)
      (*     let deps_str = *)
      (*       List.map domain_to_string deps *)
      (*       |> String.concat "," |> Printf.sprintf "[%s]" *)
      (*     in *)
      (*     let s = *)
      (*       Printf.sprintf "%s %s %s" (domain_to_string d) (MI.to_string y) *)
      (*         deps_str *)
      (*     in *)
      (*     print_endline s) *)
      (* changes; *)
      List.fold_left
        (fun ch (x, y, _) ->
          let old = find_dft bot result x in
          Hashtbl.replace result x (MI.join y old);
          ch || not (MI.less_or_equal y old))
        false changes

    let step () : bool =
      let changes = ref [] in
      todo := FormSet.singleton (program, []);
      visited := FormSet.empty;
      while not (FormSet.is_empty !todo) do
        let ((e, cxt) as form) = FormSet.min_elt !todo in
        todo := FormSet.remove form !todo;
        let new_changes = collect_changes e cxt in
        List.iter
          (fun (d, y, deps) ->
            List.iter
              (fun d ->
                match d with
                | Var (_, _) -> ()
                | Lab (l, cxt) ->
                    todo :=
                      FormSet.add (Hashtbl.find lab_expr_mapping l, cxt) !todo)
              (d :: deps))
          new_changes;
        changes := new_changes @ !changes
      done;
      (* let _ = print_endline "VISITED:" in *)
      (* FormSet.iter *)
      (*   (fun (e, _) -> print_endline (label_to_string e.label)) *)
      (*   !visited; *)
      apply_changes !changes

    let rec run () = if step () then run () else ()
  end

  let analyse (program : expr) : MI.t state =
    (* let _ = print_endline "NEWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW" in *)
    let module Instance = Worker (struct
      let program = program
    end) in
    Instance.run ();
    Instance.result
end
