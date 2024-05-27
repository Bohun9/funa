open Lang.Ast

(* Type of a context for K-CFA *)
type context = label list

let context_to_string cxt =
  cxt
  |> List.map (fun l -> label_to_string l)
  |> String.concat "," |> Printf.sprintf "[%s]"

(* Domain of the analysis *)
type domain = Lab of label * context | Var of var * context

let domain_to_string (d : domain) : string =
  match d with
  | Lab (l, cxt) ->
      Printf.sprintf "L%s %s" (string_of_int l) (context_to_string cxt)
  | Var (x, cxt) -> Printf.sprintf "V%s %s" x (context_to_string cxt)

(* State of the analysis contaning both the abstract cache and the abstract environment *)
type 'a state = (domain, 'a) Hashtbl.t

(* Type for functions for the control flow analysis *)
type func = Func of label * var * expr * context

module FuncSet = Set.Make (struct
  type t = func

  let compare = compare
end)

let funcset_to_string (s : FuncSet.t) : string =
  FuncSet.to_list s
  |> List.map (fun (Func (l, _, _, _)) -> label_to_string l)
  |> List.sort_uniq compare |> String.concat "," |> Printf.sprintf "{%s}"

(* Type for a result of the control flow analysis *)
type cfg = FuncSet.t state

(* Type of a record passed to the constraints function containing a currect context and a push function *)
type cxt_info = { cxt : context; push : label -> context }

(* Dependences of transition function *)
type dependences = domain list

(* Type for a list of changes *)
type 'a change = domain * 'a * dependences
type 'a changes = 'a change list

(* Type for specifying implicit flow constarints *)
type flow_constr_specifier = FCAll | FCAllWithoutIf | FCNone

(* Type of cache *)
type 'a cache = label -> context -> 'a
type cache_cfg = FuncSet.t cache

(* Type of environment *)
type 'a env = var -> context -> 'a

module type MonotoneInstance = sig
  (* Type of elements of the lattice *)
  type t

  (* Function used for showing results *)
  val to_string : t -> string

  (* Bottom value of the lattice that can depend on the analysed program *)
  val bot : expr -> t

  (* Join operation on the lattice *)
  val join : t -> t -> t

  (* Comparison function for checking if progress has been made *)
  val less_or_equal : t -> t -> bool

  (* This function will be called for every subexpression in the program
     and given current state of the analysis it should generate changes
     that will be included via the join operations *)
  val constraints :
    FuncSet.t cache -> expr -> t cache * t env -> cxt_info -> t changes

  (* The following options affect generating implicit constraints *)

  (*
     [Let x e1 e2] cache(l2) ⊆  cache(l)
     ...
  *)
  val gen_flow_constraints : flow_constr_specifier

  (*
     [Var x]       env(x)    ⊆  cache(l)
     [Let x e1 e2] cache(e1) ⊆  env(x)
     [App e1 e2]   cache(l2) ⊆  env(x)
  *)
  val gen_var_constraints : bool

  (* Whether to analyse branches of the if in the specific context *)
  val analyse_if_branches :
    expr * expr * expr -> t cache -> context -> bool * bool

  (* This function is a hack to obtain results of control flow analysis to be used as input to data flow analyses *)
  val to_funcset : t -> FuncSet.t
end
