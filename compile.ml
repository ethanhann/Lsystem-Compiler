(* Primary Author: Ethan Hann (eh2413) *)

open Ast
open Lsystemstd
open Str

exception RedeclarationOfStandardFunctionNotAllowedError

module StringMap = Map.Make(String);;
			
let get_prod fname = function
	  Lambda(symbols) -> "		" ^ fname ^ ".addProduction(\"lambda\", \"" ^ (String.concat "," symbols) ^ "\");\n"
	| ERule(name, symbols) -> "		" ^ fname ^ ".addProduction(\"" ^ name ^ "\", \"" ^ (String.concat "," symbols) ^ "\");\n"
	| _ -> ""

let get_term fname = function 
	  FRule(symbol, command, param) -> "		" ^ fname ^ ".addTerminal(\"" ^ symbol ^ "\", new Command(" ^ (String.uppercase command) ^ ", " ^ (string_of_expr (List.hd param)) ^ "));\n" 
	| _ -> ""
			
let translate fname rule = match rule with
	  Lambda(_) -> get_prod fname rule
	| ERule(_,_) -> get_prod fname rule
	| FRule(_,_,_) -> get_term fname rule
	| _ -> ""
			
let draw_fdecl fdecl =
	if List.mem fdecl.name Lsystemstd.func_names then 
		raise RedeclarationOfStandardFunctionNotAllowedError
	else
		let fname = fdecl.name in
			let fun_sig = "		Function " ^ fname ^ " = new Function(\"" ^ fname ^ "\");\n" in
				let lsys = fdecl.rules in match lsys with
					LSystem(alphabet, lambda, rules) -> fun_sig ^ (translate fname lambda) ^ (String.concat "" (List.map (function rule -> translate fname rule) rules))

let translate_compute_fdecl fdecl =
	if List.mem fdecl.fname Lsystemstd.func_names then 
		raise RedeclarationOfStandardFunctionNotAllowedError
	else
		let fun_sig =
			match fdecl.fname with
			  "main" -> Lsystemstd.std_render_signature
			| _ -> "	public double " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_fparam fdecl.formals) ^ "){\n" in
			fun_sig ^ "		" ^
				String.concat "		" (List.map string_of_vdecl fdecl.locals) ^
				String.concat "		" (List.map string_of_stmt fdecl.body) ^
				"	}\n"

(* Call the appropriate translation function depending on type of function. *)
let translate_fdecl = function
  CFunc(fdecl) -> translate_compute_fdecl fdecl
| DFunc(fdecl) -> draw_fdecl fdecl

let get_dfuncs = function DFunc(fdecl) -> draw_fdecl fdecl | _ -> ""

let get_cfuncs = function CFunc(fdecl) -> translate_compute_fdecl fdecl | _ -> ""

let get_dcalls = function DFunc(fdecl) -> let name = fdecl.name in "	public void " ^ name ^ "(int depth){\n" ^ "		draw(\"" ^ name ^ "\", depth);\n	}\n" | _ -> ""
			
let translate funcs prog_name verbose testmode =
	let out_chan = open_out (prog_name ^ ".java") in
		let translated_prog = 
			Lsystemstd.std_turtle1 ^ (if testmode then "true;" else "false;") ^ Lsystemstd.std_turtle2 ^ prog_name ^ Lsystemstd.std_turtle3 ^
			"public class " ^ prog_name ^ " extends Turtle {\n" ^ global_replace (Str.regexp "CLASSNAME") prog_name Lsystemstd.std_main ^ 
			"	public " ^ prog_name ^ "(){\n" ^ String.concat "" (List.map get_dfuncs funcs) ^ "		execute();\n		scale(1);\n	}\n" ^ 
			String.concat "" (List.map get_cfuncs funcs) ^
			String.concat "" (List.map get_dcalls funcs) ^ "}\n"
		in 
			let proc_status = ignore(Printf.fprintf out_chan "%s" translated_prog); 
				close_out out_chan; 
				Sys.command (Printf.sprintf "javac %s.java" prog_name) in
					match proc_status with
					  0 -> if verbose 
										then translated_prog ^ "\nCompilation successful\n" 
										else "Compilation successful\n"
					| _ -> "\nCompilation of Java bytecode unsuccessful!\n" ^
							Printf.sprintf "Javac Process Return Code: %i\n" proc_status ^
							Printf.sprintf "Compilation Command: javac %s.java\n" prog_name
