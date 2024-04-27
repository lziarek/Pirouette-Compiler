%{
  open Ast.Local
  open Ast.Choreo
%}

%token <string * (string * int)> ID
%token <int>    INT
%token <string> STRING
%token TRUE FALSE
%token UNIT_T INT_T STRING_T BOOL_T
%token FUN TYPE
%token <string * int> PLUS MINUS TIMES DIV
%token <string * int> AND OR
%token <string * int> EQ NEQ LT LEQ GT GEQ
%token LPAREN RPAREN LBRACKET RBRACKET
%token <string * int> COMMA DOT COLON SEMICOLON
%token VERTICAL UNDERSCORE
%token COLONEQ ARROW TILDE_ARROW
%token LET IN
%token IF THEN ELSE
%token FST SND LEFT RIGHT
%token MATCH WITH
%token <string * int> EOF /* filename */

%type <Ast.Choreo.program> program
%type <Ast.Choreo.decl_block> decl_block
%type <Ast.Choreo.statement> statement
%type <Ast.Choreo.choreo_expr> choreo_expr
%type <Ast.Choreo.pattern> pattern
%type <Ast.Choreo.choreo_type> choreo_type
%type <Ast.Local.local_expr> local_expr
%type <Ast.Local.local_pattern> local_pattern
%type <Ast.Local.local_type> local_type
%type <Ast.Local.bin_op> bin_op
%type <Ast.Local.value> value
%type <Ast.Local.loc_id> loc_id
%type <Ast.Local.var_id> var_id
%type <Ast.Local.sync_label> sync_label

%nonassoc IN
%nonassoc VERTICAL
%nonassoc FST SND LEFT RIGHT
%right OR
%right AND
%left EQ NEQ LT LEQ GT GEQ
%right ARROW
%left PLUS MINUS
%left TIMES DIV
%left DOT


%start program

%%

program:
  | decl_block EOF { Prog ($1, $2) } /* filename at EOF*/

decl_block:
  | list(statement) { $1 }

/* TODO: Removing the need for semicolons */
statement:
  | pattern COLON choreo_type SEMICOLON        { Decl ($1, $3, $2) } // metainfo at $2
  | pattern COLONEQ choreo_expr SEMICOLON      { Assign ($1, $3, $4) } // metainfo at $4
  | TYPE var_id COLONEQ choreo_type SEMICOLON? { TypeDecl ($2, $4) }

/* Associativity increases from expr to expr3, with each precedence level falling through to the next. */
choreo_expr:
  | IF choreo_expr THEN choreo_expr ELSE choreo_expr                             { If ($2, $4, $6) }
  | LET decl_block IN choreo_expr                                                { Let ($2, $4) }
  | FUN var_id ARROW choreo_expr                                                 { FunDef ($2, $4) }
  | FST choreo_expr                                                              { Fst $2 }
  | SND choreo_expr                                                              { Snd $2 }
  | LEFT choreo_expr                                                             { Left $2 }
  | RIGHT choreo_expr                                                            { Right $2 }
  | LPAREN choreo_expr COMMA choreo_expr RPAREN                                  { Pair ($2, $4) }
  | MATCH choreo_expr WITH nonempty_list(case)                                   { Match ($2, $4) }
  | loc_id LBRACKET sync_label RBRACKET TILDE_ARROW loc_id SEMICOLON choreo_expr { Sync ($1, $3, $6, $8) }
  | choreo_expr1 TILDE_ARROW loc_id DOT var_id SEMICOLON choreo_expr             { Let ([Assign (LocPatt ($3, Var $5), Send ($1, $3), $6)], $7) } // metainfo at $6
  | choreo_expr1                                                                 { $1 }

choreo_expr1:
  | choreo_expr1 TILDE_ARROW loc_id                                              { Send ($1, $3) }
  | choreo_expr2                                                                 { $1 }

choreo_expr2:
  | choreo_expr2 choreo_expr3                                                    { FunApp ($1, $2) }
  | choreo_expr3                                                                 { $1 }

choreo_expr3:
  | LPAREN RPAREN                                                                { Unit }
  | var_id                                                                       { Var $1 }
  | loc_id DOT local_expr                                                        { LocExpr ($1, $3) }
  | LPAREN choreo_expr RPAREN                                                    { $2 }

local_expr:
  | LPAREN RPAREN                                   { Unit }
  | value                                           { Val $1 }                                                                    
  | var_id                                          { Var $1 }
  | local_expr bin_op local_expr                    { BinOp ($1, $2, $3) }
  | LET var_id COLONEQ local_expr IN local_expr     { Let ($2, $4, $6) }
  | LPAREN local_expr COMMA local_expr RPAREN       { Pair ($2, $4) }
  | FST local_expr                                  { Fst $2 }
  | SND local_expr                                  { Snd $2 }
  | LEFT local_expr                                 { Left $2 }
  | RIGHT local_expr                                { Right $2 }
  | MATCH local_expr WITH nonempty_list(local_case) { Match ($2, $4) }
  | LPAREN local_expr RPAREN                        { $2 }

pattern:
  | UNDERSCORE                          { Default }
  | var_id                              { Var $1 }
  | loc_id DOT local_pattern            { LocPatt ($1, $3) }
  | LPAREN pattern COMMA pattern RPAREN { Pair ($2, $4) }
  | LEFT pattern                        { Left $2 }
  | RIGHT pattern                       { Right $2 }
  | LPAREN pattern RPAREN               { $2 }
  
local_pattern:
  | UNDERSCORE                                      { Default }
  | value                                           { Val $1 }
  | var_id                                          { Var $1 }
  | LPAREN local_pattern COMMA local_pattern RPAREN { Pair ($2, $4) }
  | LEFT local_pattern                              { Left $2 }
  | RIGHT local_pattern                             { Right $2 }
  | LPAREN local_pattern RPAREN                     { $2 }


(** [choreo_type] parses choreography types and constructs corresponding AST nodes.
    - Returns: An AST node representing the choreography type.
*)
choreo_type:
  | UNIT_T                        { TUnit }
  | loc_id DOT local_type         { TLoc ($1, $3) }
  | choreo_type ARROW choreo_type { TSend ($1, $3) }
  | choreo_type TIMES choreo_type { TProd ($1, $3) }
  | choreo_type PLUS choreo_type  { TSum ($1, $3) }
  | LPAREN choreo_type RPAREN     { $2 }


(** [local_type] parses local types and constructs corresponding AST nodes.
    - Returns: An AST node representing the local type.
*)  

local_type:
  | UNIT_T                      { TUnit }
  | INT_T                       { TInt }
  | STRING_T                    { TString }
  | BOOL_T                      { TBool }
  | local_type TIMES local_type { TProd ($1, $3) }
  | local_type PLUS local_type  { TSum ($1, $3) }
  | LPAREN local_type RPAREN    { $2 }
  
(** [loc_id] parses an identifier for a location and constructs a corresponding AST node.

    - Returns: A `LocId` node containing the identifier and its metadata.
    - Example: Parsing an `ID` token results in a `LocId` node with the identifier and associated metadata.
*)
loc_id:
  | ID { let (id, metainfo) = $1 in LocId (id, metainfo) }

(** [var_id] parses an identifier for a variable and constructs a corresponding AST node.

    - Returns: A `VarId` node containing the identifier and its metadata.
    - Example: Parsing an `ID` token results in a `VarId` node with the identifier and associated metadata.
*)
var_id:
  | ID { let (id, metainfo) = $1 in VarId (id, metainfo) }

sync_label:
  | ID { let (id, metainfo) = $1 in LabelId (id, metainfo) }


value:
  | INT    { `Int $1 }
  | STRING { `String $1 }
  | TRUE   { `Bool true }
  | FALSE  { `Bool false }

(** [case] parses case expressions for choreography expressions and constructs corresponding AST nodes.

    Each case is defined by a pattern and an expression, separated by an arrow.

    - Returns: A tuple containing the parsed pattern and the corresponding choreography expression.
    - Example: Parsing a vertical bar, a pattern, an arrow, and a choreography expression results in a tuple of the pattern and expression.
*)

%inline case:
  | VERTICAL pattern ARROW choreo_expr1     { ($2, $4) }

(** [local_case] parses case expressions for local expressions and constructs corresponding AST nodes.

    Similar to [case], but specifically for local expressions within a choreography.

    - Returns: A tuple containing the parsed local pattern and the corresponding local expression.
    - Example: Parsing a vertical bar, a local pattern, an arrow, and a local expression results in a tuple of the local pattern and expression.
*)

%inline local_case:
  | VERTICAL local_pattern ARROW local_expr { ($2, $4) }

(** [bin_op] parses binary operators and constructs corresponding AST nodes.

    Each operator is associated with a specific constructor that takes location information as an argument.

    - Returns: An AST node representing the binary operation.
    - Example: Parsing the token PLUS with location info at position 1 results in [Plus ($1)].
*)

%inline bin_op:
  | PLUS  { Plus ($1) }
  | MINUS { Minus ($1) }
  | TIMES { Times ($1) }
  | DIV   { Div ($1) }
  | AND   { And ($1) }
  | OR    { Or ($1) }
  | EQ    { Eq ($1) }
  | NEQ   { Neq ($1) }
  | LT    { Lt ($1) }
  | LEQ   { Leq ($1) }
  | GT    { Gt ($1) }
  | GEQ   { Geq ($1) }