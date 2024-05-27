(* Expressions are terms with labels *)

type var = string
type label = int

module VarSet = Set.Make (String)

let var_to_string x = x
let label_to_string = string_of_int

type unop = |

let unop_to_string (op : unop) : string = match op with _ -> .

type binop = BINOP_Add | BINOP_Mul | BINOP_Sub
type relop = RELOP_Gt | RELOP_Lt

let binop_to_string (op : binop) : string =
  match op with BINOP_Add -> "+" | BINOP_Mul -> "*" | BINOP_Sub -> "-"

let relop_to_string (op : relop) : string =
  match op with RELOP_Gt -> "&gt;" | RELOP_Lt -> "&lt;"

type term =
  | TInt of int
  | TBool of bool
  | TVar of var
  | TIf of term * term * term
  | TLet of var * term * term
  | TApp of term * term
  | TLam of var * term
  | TRec of var * var * term
  | TUnop of unop * term
  | TBinop of binop * term * term
  | TRelop of relop * term * term

type expr = { data : expr_data; label : label }

and expr_data =
  | EInt of int
  | EBool of bool
  | EVar of var
  | EIf of expr * expr * expr
  | ELet of var * expr * expr
  | EApp of expr * expr
  | ELam of var * expr
  | ERec of var * var * expr
  | EUnop of unop * expr
  | EBinop of binop * expr * expr
  | ERelop of relop * expr * expr

let rec _app_labels (e : expr) : label list =
  match e.data with
  | EInt _ | EBool _ | EVar _ -> []
  | EApp (e1, e2) -> [ e.label ] @ _app_labels e1 @ _app_labels e2
  | EIf (e1, e2, e3) -> _app_labels e1 @ _app_labels e2 @ _app_labels e3
  | ELet (_, e1, e2) | EBinop (_, e1, e2) | ERelop (_, e1, e2) ->
      _app_labels e1 @ _app_labels e2
  | ELam (_, e1) | ERec (_, _, e1) | EUnop (_, e1) -> _app_labels e1

let rec _labels (e : expr) : label list =
  let labs =
    match e.data with
    | EInt _ | EBool _ | EVar _ -> []
    | EApp (e1, e2) -> _labels e1 @ _labels e2
    | EIf (e1, e2, e3) -> _labels e1 @ _labels e2 @ _labels e3
    | ELet (_, e1, e2) | EBinop (_, e1, e2) | ERelop (_, e1, e2) ->
        _labels e1 @ _labels e2
    | ELam (_, e1) | ERec (_, _, e1) | EUnop (_, e1) -> _labels e1
  in
  e.label :: labs

let app_labels (e : expr) : label list = List.sort compare (_app_labels e)
let labels (e : expr) : label list = List.sort compare (_labels e)

let rec _vars_of_expr (e : expr) : var list =
  match e.data with
  | EInt _ | EBool _ -> []
  | EVar x -> [ x ]
  | EApp (e1, e2) -> _vars_of_expr e1 @ _vars_of_expr e2
  | EIf (e1, e2, e3) -> _vars_of_expr e1 @ _vars_of_expr e2 @ _vars_of_expr e3
  | ELet (x, e1, e2) -> [ x ] @ _vars_of_expr e1 @ _vars_of_expr e2
  | EBinop (_, e1, e2) | ERelop (_, e1, e2) ->
      _vars_of_expr e1 @ _vars_of_expr e2
  | ELam (x, e1) -> [ x ] @ _vars_of_expr e1
  | ERec (f, x, e1) -> [ f; x ] @ _vars_of_expr e1
  | EUnop (_, e1) -> _vars_of_expr e1

let vars_of_expr (e : expr) : var list =
  List.sort_uniq compare (_vars_of_expr e)

let rec free_vars_of_expr (e : expr) : VarSet.t =
  match e.data with
  | EInt _ | EBool _ -> VarSet.empty
  | EVar x -> VarSet.singleton x
  | EApp (e1, e2) | EBinop (_, e1, e2) | ERelop (_, e1, e2) ->
      VarSet.union (free_vars_of_expr e1) (free_vars_of_expr e2)
  | EIf (e1, e2, e3) ->
      VarSet.union
        (VarSet.union (free_vars_of_expr e1) (free_vars_of_expr e2))
        (free_vars_of_expr e3)
  | EUnop (_, e1) -> free_vars_of_expr e1
  | ELet (x, e1, e2) ->
      VarSet.remove x
        (VarSet.union (free_vars_of_expr e1) (free_vars_of_expr e2))
  | ELam (x, e1) -> VarSet.remove x (free_vars_of_expr e1)
  | ERec (f, x, e1) -> VarSet.remove f (VarSet.remove x (free_vars_of_expr e1))
