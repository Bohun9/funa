open Ast

let parse_from_string (source : string) : term =
  let lexbuf = Lexing.from_string source in
  try Parser.file Lexer.token lexbuf
  with Parser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!"
      (Lexing.lexeme_start lexbuf);
    failwith ""

let gen_label =
  let x = ref 0 in
  fun () ->
    incr x;
    !x

let rec attach_labels (t : term) : expr =
  let e =
    match t with
    | TInt n -> EInt n
    | TBool b -> EBool b
    | TVar x -> EVar x
    | TIf (t1, t2, t3) ->
        EIf (attach_labels t1, attach_labels t2, attach_labels t3)
    | TLet (x, t1, t2) -> ELet (x, attach_labels t1, attach_labels t2)
    | TApp (t1, t2) -> EApp (attach_labels t1, attach_labels t2)
    | TLam (x, t) -> ELam (x, attach_labels t)
    | TRec (f, x, t) -> ERec (f, x, attach_labels t)
    | TUnop (op, t) -> EUnop (op, attach_labels t)
    | TBinop (op, t1, t2) -> EBinop (op, attach_labels t1, attach_labels t2)
    | TRelop (op, t1, t2) -> ERelop (op, attach_labels t1, attach_labels t2)
  in
  { data = e; label = gen_label () }
