{
open Parser

let kw_map = [
  "if", IF;
  "then", THEN;
  "else", ELSE;
  "fn", FN;
  "rec", REC;
  "let", LET;
  "in", IN;
] |> List.to_seq |> Hashtbl.of_seq

let symbols_map = [
  "(", LPAREN;
  ")", RPAREN;
  "=", OP_ASSIGN;
  "+", OP_ADD;
  "*", OP_MUL;
  "-", OP_MINUS;
  ">", OP_GT;
  "<", OP_LT;
  ">=", OP_GE;
  "<=", OP_LE;
  "=>", OP_ARROW;
  "!", OP_NOT;
] |> List.to_seq |> Hashtbl.of_seq

let make_id s = 
  try
    Hashtbl.find kw_map s
  with
    Not_found -> IDENTIFIER s

let make_symbol s = 
  Hashtbl.find symbols_map s
}

let digits = ['0'-'9']
let int = digits+
let identifier = ['a'-'z' '_'] ['a'-'z' '_' '0'-'9']*
let symbols = ['(' ')' '=' '+' '*' '-' '>' '<' '!'] | "=>" | "<=" | ">="

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | int as i { INT (int_of_string i) }
  | identifier as s { make_id s }
  | symbols as s { make_symbol s }
  | eof { EOF }
