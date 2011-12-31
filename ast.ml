(* Primary Author: Jervis Muindi (jjm2190) *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Pow
type nop = NAdd | NSub | NMult | NDiv (*the four normal operators*)

type bv = True | False
type bop = And| Or 
type eop = BEqual | BNeq 
type rop = BLess | BLeq | BGreater | BGeq

type mop = MTimes | MDivide | MMod (*multiplicative expr ops*)
type aop = AAdd | ASub (*additve expr ops*)

type vop = VAdd | VSub | VMult | VDiv
type dt = StringType | DoubleType | IntType | BooleanType (*Data types in our language*)

type fparam = FParam of dt * string (*Type to hold a Formal parameter, e.g. int x*)

type vdecl = VDecl of dt * string * string  (*DataType, Name, Value*)

type arithexpr = (*key*)
	| ALiteral of int
	| AId of string
	| AFloat of float

type varexpr = (*key*)
	| VLiteral of int
	| VId of string
	| VFloat of float
	| VStringLit of string
	| VBoolLit of bool
	| VBinop of varexpr *  vop * varexpr

type expr =
	  Literal of int
	| Float of float
	| Boolean of bool
	| String of string
	| Id of string
	| Bracket of expr
	| Binop of expr * op * expr
	| Assign of string * expr
	| Call of string * expr list
	| Noexpr
	| BVal of bv (*boolean value : true/false*)
	| RExpr of expr * rop * expr (*relational expresion : < <= > >=*)
	| EExpr of expr * eop * expr (*equality expression : == !=*)
	| BExpr of expr * bop * expr (*boolean compound expression : && || *)

type stmt =
	  Block of stmt list 
	| Expr of expr
	(*| Decl of dt * string * string*)
	| Return of expr
	| If of expr * stmt * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt

type alphabet =
	| Alphabet of string list

type turtle_param =
	TurtleParam of varexpr 

type rule =
	| Lambda of string list (*start rule: lambda ->  production_rule *)
	| ERule of string * string list (*Expansion rule : alphabet_symbol -> Expansion.*) 
	| FRule of string * string * expr list (*Function rule : name | turtle function name | parameters. E.g f = turtle_move(100)*)
	| EmptyFRule of string (*the empty function rule. E.g. A = ;*) 

type lsystem =
	LSystem of alphabet * rule * rule list (*Alphabet | Lambda rule | The other rules*)

type lfunc_decl = {
	name : string;
	formal : fparam list;
	rules : lsystem;
}

type func_decl = {
	fname : string;
	formals : fparam list;
	locals : vdecl list;
	body : stmt list;
}

type func =
	| CFunc of func_decl (*compute function*)
	| DFunc of lfunc_decl (*draw function*)

type program = func list

let string_of_var_dec (a,b,c) = a ^ b ^ c 

let string_of_vop  = function
	| VAdd -> "+"
	| VSub -> "-"
	| VMult-> "*"
	| VDiv -> "/"

let string_of_arithexpr  = function
	| ALiteral(i) -> string_of_int i
	| AId(s) -> s
	| AFloat(f) -> string_of_float f

let rec string_of_varexpr = function
	| VLiteral(i) -> string_of_int i
	| VId(s) -> s
	| VFloat(f) -> string_of_float f
	| VStringLit(s) -> s
	| VBoolLit(b) -> string_of_bool b 
	| VBinop(v1,op,v2) -> string_of_varexpr v1 ^ " " ^ string_of_vop op ^ " " ^ string_of_varexpr v2  

let string_of_dt = function
	  StringType -> "string"
	| DoubleType -> "double"
	| IntType -> "int"
	| BooleanType -> "boolean"

let string_of_bop = function
	| And -> "&&"
	| Or -> "||"

let string_of_rop = function 
	| BLess -> "<"
	| BLeq -> "<="
	| BGreater -> ">"
	| BGeq -> ">="

let string_of_eop = function
	| BEqual -> "=="
	| BNeq -> "!="

let string_of_bv = function 
	| True -> "true"
	| False -> "false"

let string_of_op = function
	  Add -> "+" 
	| Sub -> "-" 
	| Mult -> "*" 
	| Div -> "/"
	| Equal -> "==" 
	| Neq -> "!="
	| Less -> "<" 
	| Leq -> "<=" 
	| Greater -> ">" 
	| Geq -> ">=" 
	| Pow -> "^"

let rec string_of_expr = function
	  Literal(l) -> string_of_int l
	| Boolean(b) -> string_of_bool b
	| Float(f) -> string_of_float f
	| String(s) -> s
	| Id(s) -> s
	| Binop(e1, o, e2) ->
			begin
				match o with 
					| Pow -> "Math.pow(" ^ string_of_expr e1 ^ " , " ^ string_of_expr e2 ^ ")"
					| _ -> 
							string_of_expr e1 ^ " " ^ (match o with
								  Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
								| Equal -> "==" | Neq -> "!="
								| Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" | Pow -> "^")  
							^ " " ^ string_of_expr e2
			end
	| Assign(v, e) -> v ^ " = " ^ string_of_expr e
	| Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	| Noexpr -> ""
	| BVal(v) -> string_of_bv v 
	| RExpr(e1,o,e2) -> string_of_expr e1 ^ " " ^ string_of_rop o ^ " " ^ string_of_expr e2
	| EExpr(e1,o,e2) -> string_of_expr e1 ^ " " ^ string_of_eop o ^ " " ^ string_of_expr e2
	| BExpr(e1,o,e2) -> string_of_expr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_expr e2
	| Bracket(e1) -> " ( " ^ string_of_expr e1 ^ " ) " 

let rec string_of_stmt = function
	  Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) -> string_of_expr expr ^ ";\n";
	| Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
	| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
	| If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
	| For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") " ^ string_of_stmt s
	| While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl = function 
	VDecl(dtt, nm, v) ->  string_of_dt dtt ^ " " ^ nm ^ " = " ^ v ^ ";\n"

let string_of_alphabet = function
	Alphabet(string_list) -> String.concat " " string_list

let string_of_lambdarule = function
	| Lambda(string_list) ->  "lambda -> " ^ String.concat " " string_list
	| _ -> ""  (*output nothing if not a lambda rule*)

let string_of_lambdarule_value = function
	|  Lambda(string_list) ->  String.concat " " string_list
	| _ -> ""  (*output nothing if not a lambda rule*)

let string_of_rule = function
	| Lambda(string_list) ->  "lambda -> " ^ String.concat " " string_list
	| ERule(name, string_list) -> name ^ " -> " ^ String.concat " " string_list ^ "\n"
	| FRule(name, fname, params) -> name ^ " = " ^ fname ^ "(" ^ String.concat "," (List.map string_of_expr params)  ^ ")" ^ "\n"
	| EmptyFRule(s) -> s ^ " = " ^ "\n"

let string_of_fparam = function
	FParam(dt,s) -> string_of_dt dt ^ " " ^ s

let string_of_lsystem  = function
	LSystem(a,s,rl) ->   string_of_alphabet a ^ "\n" ^string_of_lambdarule s ^ "\n" ^ String.concat "" (List.map string_of_rule rl) 
 
let string_of_dfunc (func) = 
	"Function name : " ^ func.name ^ "\n" ^ 
	"Formal Parameter(s) : " ^ String.concat "," (List.map string_of_fparam func.formal) ^ "\n" ^
	"Lsystem: " ^ "\n" ^ string_of_lsystem func.rules	
 
let string_of_fdecl  = function
	| CFunc(fdecl) -> 
			"\ndef compute " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_fparam fdecl.formals) ^ ") {\n" ^
			String.concat "" (List.map string_of_vdecl fdecl.locals) ^
			String.concat "" (List.map string_of_stmt fdecl.body) ^
			"}\n"
	| DFunc(fdecl) -> "\ndef draw " ^ fdecl.name ^ "(" ^ String.concat ", " (List.map string_of_fparam fdecl.formal) ^ ") {\n" ^
		string_of_lsystem fdecl.rules ^ "}\n"

let string_of_program (funcs) = String.concat "\n" (List.map string_of_fdecl funcs)	
