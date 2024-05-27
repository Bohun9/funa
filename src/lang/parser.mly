%{
  open Ast
%}

%type <Ast.term> file
%start file

%token EOF
%token <string> IDENTIFIER
%token <int> INT

%token IF THEN ELSE FN REC LET IN
%token LPAREN RPAREN
%token OP_ASSIGN OP_ADD OP_MUL OP_MINUS OP_ARROW OP_GREATER OP_LESS

%%

expr
  : FN IDENTIFIER OP_ARROW expr { TLam($2, $4) }
  | FN REC IDENTIFIER IDENTIFIER OP_ARROW expr { TRec($3, $4, $6) }
  | LET IDENTIFIER OP_ASSIGN expr IN expr { TLet($2, $4, $6) }
  | IF expr THEN expr ELSE expr { TIf($2, $4, $6) }
  | expr_rel { $1 }

op_rel
  : OP_GREATER { RELOP_Gt }
  | OP_LESS { RELOP_Lt }

expr_rel
  : expr_add op_rel expr_add { TRelop($2, $1, $3) }
  | expr_add { $1 }

op_add : OP_ADD { BINOP_Add }
       | OP_MINUS { BINOP_Sub }
op_mul : OP_MUL { BINOP_Mul }

expr_add
  : expr_add op_add expr_mul { TBinop($2, $1, $3) }
  | expr_mul { $1 }

expr_mul
  : expr_mul op_mul expr_app { TBinop($2, $1, $3) }
  | expr_app { $1 }

expr_app
  : expr_app expr_simpl { TApp($1, $2) }
  | expr_simpl { $1 }

expr_simpl
  : INT { TInt $1 }
  | IDENTIFIER { TVar $1 }
  | LPAREN expr RPAREN { $2 }

file : expr EOF { $1 }


%%
