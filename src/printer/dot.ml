open Lang.Ast
open Framework.Api

module MakeDotProgram (E : sig
  val e : expr
  val results : (domain, string) Hashtbl.t option
end) =
struct
  open E

  let label_results : (label, (context * string) list) Hashtbl.t =
    Hashtbl.create 127

  let _ =
    match results with
    | None -> ()
    | Some results ->
        Hashtbl.iter
          (fun d y ->
            match d with
            | Lab (l, cxt) ->
                let ys =
                  match Hashtbl.find_opt label_results l with
                  | Some x -> (cxt, y) :: x
                  | None -> [ (cxt, y) ]
                in
                Hashtbl.replace label_results l ys
            | Var (_, _) -> ())
          results

  let results_rows (l : label) : string =
    match results with
    | None -> ""
    | Some _ ->
        List.fold_left
          (fun ac (cxt, y) ->
            Printf.sprintf "%s    <TR> <TD>%s</TD> <TD>%s</TD> </TR>\n" ac
              (context_to_string cxt) y)
          ""
          (Hashtbl.find label_results l)

  let node_stmt label text =
    Printf.sprintf
      {|
  %d [label=<
          <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
              <TR>
                  <TD BGCOLOR="lightblue">%d</TD>
                  <TD>%s</TD>
              </TR>
              %s
          </TABLE>>];
  |}
      label label text (results_rows label)

  let node_text (e : expr) : string =
    match e.data with
    | EInt i -> string_of_int i
    | EBool b -> string_of_bool b
    | EVar x -> x
    | EIf (_, _, _) -> "if"
    | ELet (x, _, _) -> "let " ^ x
    | EApp (_, _) -> "app"
    | EBinop (op, _, _) -> binop_to_string op
    | EUnop (op, _) -> unop_to_string op
    | ELam (x, _) -> "fn " ^ x
    | ERec (f, x, _) -> Printf.sprintf "fn rec %s %s" f x

  let rec build_node_list (e : expr) : string list =
    let cur = node_stmt e.label (node_text e) in
    cur
    ::
    (match e.data with
    | EInt _ | EBool _ | EVar _ -> []
    | EIf (e1, e2, e3) ->
        build_node_list e1 @ build_node_list e2 @ build_node_list e3
    | ELet (_, e1, e2) | EApp (e1, e2) | EBinop (_, e1, e2) ->
        build_node_list e1 @ build_node_list e2
    | ELam (_, e1) | ERec (_, _, e1) | EUnop (_, e1) -> build_node_list e1)

  let edge_stmt ?(text = "") (l1 : label) (l2 : label) : string =
    if text <> "" then
      Printf.sprintf "  %s -> %s [label=%s];\n" (string_of_int l1)
        (string_of_int l2) text
    else Printf.sprintf "  %s -> %s;\n" (string_of_int l1) (string_of_int l2)

  let rec build_edge_list (e : expr) : string list =
    match e.data with
    | EInt _ | EBool _ | EVar _ -> []
    | EIf (e1, e2, e3) ->
        edge_stmt e.label e1.label :: edge_stmt e.label e2.label
        :: edge_stmt e.label e3.label :: build_edge_list e1
        @ build_edge_list e2 @ build_edge_list e3
    | ELet (_, e1, e2) ->
        edge_stmt ~text:"\"=\"" e.label e1.label
        :: edge_stmt ~text:"in" e.label e2.label
        :: build_edge_list e1
        @ build_edge_list e2
    | EApp (e1, e2) | EBinop (_, e1, e2) ->
        edge_stmt e.label e1.label :: edge_stmt e.label e2.label
        :: build_edge_list e1
        @ build_edge_list e2
    | ELam (_, e1) | ERec (_, _, e1) | EUnop (_, e1) ->
        edge_stmt e.label e1.label :: build_edge_list e1

  let node_block = String.concat "" (build_node_list e)
  let edge_block = String.concat "" (build_edge_list e)

  let dot_program =
    Printf.sprintf {|
digraph FunProgram {
  node [shape=plain];
  %s

  %s
} |}
      node_block edge_block
end
