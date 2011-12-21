(* Primary Author: Jervis Muindi (jjm2190) *)
{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let punc = ['~' '`' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '-' '+' '=' ',' '.' '?' '/' '<' '>' ':' '''  ';' '{' '}' '[' ']' '|' ' ']
(*Escape character sequences
  "\\\"" -> "[ \" ]" -> a single double quote 
	"\\\\" -> '\\' -> a backslash
	"\\n" -> \n -> new line
	"\\t" -> \t -> tab char
*)
let esp =   "\\\"" | "\\\\" | "\\n" | "\\t" (*Escape characters : see comment above*)
let exp = 'e'('+'|'-')?['0'-'9']+
let float = '-'? (digit)+ ('.' (digit)* exp?|exp)
let stringlit = '"' (letter | digit | punc | esp)*  '"'
let negative_int = '-'(digit)+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| '#'     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| ':'      { COLON }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "&&"     { AND }
| "||"     { OR }
| '^'      { POW }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "->"     { ARROW }
| "alphabet" { ALPHABET }
| "boolean" { BOOLEAN }
| "def"     { DEF }
| "compute" { COMPUTE }
| "draw" { DRAW }
| "double" { DOUBLE }
| "false"  { FALSE }
| "true"   { TRUE }
| "if"     { IF }
| "else"   { ELSE }
| "int"    { INT } 
| "lambda" { LAMBDA}
| "return" { RETURN } 
| "rules"  { RULES }
| "string" { STRING }
| "while"  { WHILE }
| (digit)+ as lxm { LITERAL(int_of_string lxm) }
| negative_int as lxm { LITERAL(int_of_string lxm) } (*negative integer*)
| letter as lxm { LETTER(String.make 1 lxm)  } (*converts lxm to a string*)
| letter (letter | digit | '_')* as lxm { ID(lxm) }
| float as lxm { FLOAT(float_of_string lxm) }
| stringlit as lxm { STRINGLIT(lxm) }
| eof { EOF }
| _ as char { raise (Failure("Illegal character: " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf } (*Comments are in effect until the end of the line*)
| _    { comment lexbuf }
