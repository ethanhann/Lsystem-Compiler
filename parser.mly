%{ 
(* Primary Author: Jervis Muindi (jjm2190) *)	
open Ast
let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA COLON
%token PLUS MINUS TIMES DIVIDE POW ASSIGN 
%token EQ NEQ LT LEQ GT GEQ
%token AND OR
%token BOOLEAN DOUBLE STRING INT
%token FALSE TRUE
%token ALPHABET LAMBDA RULES 
%token RETURN IF ELSE WHILE 
%token DEF COMPUTE DRAW
%token ARROW
%token LETTER
%token <int> LITERAL
%token <float> FLOAT
%token <string> STRINGLIT
%token <string> ID
%token <string> LETTER
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right POW 

%start program
%type <Ast.program> program


%%

program:
   /* nothing */ { [] }
 | program fdecl {  ($2 :: $1) }

fdecl:
	  DEF COMPUTE id LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE 
			{
				CFunc({ 
					fname = $3;
					formals = $5;
					locals = List.rev $8;
					body = List.rev $9 
				})
			}
	| DEF DRAW id LPAREN formals_opt RPAREN LBRACE rules RBRACE
			{
				DFunc({
					name = $3;
					formal = $5;
					rules = $8;
				})
			}

id:
	  ID     { $1 }
	| LETTER { $1 }

alphabet_list:
	  LETTER                     { [$1] }
	| alphabet_list COMMA LETTER { $3 :: $1 }

alphabet:
	ALPHABET COLON LPAREN alphabet_list RPAREN SEMI { Alphabet($4) }

production_list: /*the RHS of a production rule*/
	  LETTER                 { [$1] }
	| production_list LETTER { $2 :: $1 }

turtle_func_paramlist_opt:
	  /* nothing */         { [] }
	| turtle_func_paramlist { List.rev $1 }  

turtle_func_paramlist:
	  expr                             { [$1] }
	| turtle_func_paramlist COMMA expr { $3 :: $1 }
		
erule: /*expansion rule*/
	LETTER ARROW production_list SEMI { ERule($1, List.rev $3) } /*Reverse it so that we read in list in the right order going from left to right*/

frule: /*rule that specifies */
	  LETTER ASSIGN SEMI                                            { EmptyFRule($1) } /*the empty rule. e.g A = ;*/
	| LETTER ASSIGN ID LPAREN turtle_func_paramlist_opt RPAREN SEMI { FRule($1, $3, $5)   }

lambdarule:
	LAMBDA ARROW production_list SEMI {Lambda(List.rev $3) } /*Reverse it so that we read in list in the right order going from left to right*/ 

rule: 
	  erule { $1 }
	| frule { $1}

rule_list:
	  rule            { [$1] }
	| rule_list rule  { $2 :: $1 }

rules:
	alphabet RULES COLON LBRACE lambdarule rule_list RBRACE { LSystem($1, $5, List.rev $6) } /*Apply List.rev so that the rules are printed in the right order going from top to bottom as they were originally entered.*/

datatype:
	  BOOLEAN { BooleanType }
	| INT     { IntType }
	| DOUBLE  { DoubleType }
	| STRING  { StringType }

formals_opt:
	  /* nothing */ { [] }
	| formal_list   { List.rev $1 }

formal_list:
	  datatype id                   { [FParam($1, $2)] }
	| formal_list COMMA datatype id { FParam($3, $4) :: $1 }

vdecl_list:
	  /* nothing */    { [] }
	| vdecl_list vdecl { $2 :: $1 }
  
vdecl:
	datatype id ASSIGN expr SEMI { VDecl($1, $2, string_of_expr $4) }

stmt_list:
	  /* No empty block allowed */ { [] } 
	| stmt_list stmt               { $2 :: $1 }

stmt:
	  expr SEMI                               { Expr($1) }
	| RETURN expr SEMI                        { Return($2) }
	| LBRACE stmt_list RBRACE                 { Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
	| WHILE LPAREN expr RPAREN stmt           { While($3, $5) }

expr: /*a primary expression*/
	  LITERAL                      { Literal($1) }
	| STRINGLIT                    { String($1) }
	| FLOAT                        { Float($1) }
	| id                           { Id($1) }
	| expr PLUS  expr              { Binop($1, Add,   $3) }
	| expr MINUS  expr             { Binop($1, Sub,   $3) }
	| expr TIMES  expr             { Binop($1, Mult,  $3) }
	| expr DIVIDE expr             { Binop($1, Div,   $3) }
	| expr POW expr                { Binop($1, Pow,   $3) }
/* Boolean expression part*/ 
	| TRUE                         { BVal(True) }
	| FALSE                        { BVal(False) }
	| expr EQ expr                 { EExpr($1, BEqual, $3) }
	| expr NEQ expr                { EExpr($1, BNeq, $3) }
	| expr GT expr                 { RExpr($1, BGreater, $3) }
	| expr GEQ expr                { RExpr($1, BGeq, $3) }
	| expr LT expr                 { RExpr($1, BLess, $3) }
	| expr LEQ expr                { RExpr($1, BLeq, $3) }
	| expr AND expr                { BExpr($1, And, $3) }
	| expr OR expr                 { BExpr($1, Or, $3) }
	| id ASSIGN expr               { Assign($1, $3) }
  | id LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN           { Bracket($2) }

actuals_opt:
	  /* nothing */ { [] }
	| actuals_list  { List.rev $1 }

actuals_list:
	  expr                    { [$1] }
	| actuals_list COMMA expr { $3 :: $1 }
