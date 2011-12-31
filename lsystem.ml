(* Primary Author: Ethan Hann (eh2413) *)
(* Command Line Interface *)

(* Possible actions for the compiler. *)
type action = Ast | Compile | SA

(* Custom exceptions. *)
exception NoInputFile
exception InvalidArgument

(* Compiler usage instructions. *)
let usage = Printf.sprintf "Usage: lsystem [-a|-s|-c] SOURCE_FILE [-t|-v]"

(* Get the name of the program from the file name. *)						
let get_prog_name source_file_path =
	let split_path = (Str.split (Str.regexp_string "/") source_file_path) in
	let file_name = List.nth split_path ((List.length split_path) - 1) in
	let split_name = (Str.split (Str.regexp_string ".") file_name) in
		List.nth split_name ((List.length split_name) - 2)

(* Main entry point *)
let _ =
	try
		let action = if Array.length Sys.argv > 1 then
			match Sys.argv.(1) with 
					| "-a" -> Ast
					| "-s" -> SA (*semantic analysis testing*) 
					| "-c" -> Compile
					| _ -> raise InvalidArgument
			else raise InvalidArgument in
		let prog_name = 
			if Array.length Sys.argv > 2 then
				get_prog_name Sys.argv.(2)
			else raise NoInputFile in
		let verbose = 
			if Array.length Sys.argv > 3 then
				match Sys.argv.(3) with 
				| "-v" -> true
				| _ -> false
			else false in
		let testmode = 
			if Array.length Sys.argv > 3 then
				match Sys.argv.(3) with 
				| "-t" -> true
				| _ -> false
			else false in	
		let input_chan = open_in Sys.argv.(2) in
  	let lexbuf = Lexing.from_channel input_chan in
  	let reversed_program = Parser.program Scanner.token lexbuf in
		let program = List.rev reversed_program in
		match action with 
			| Ast -> let listing = Ast.string_of_program program in print_string listing
			| SA -> ignore (Semantic.check_program program);
			| Compile -> 
					if Semantic.check_program program 
						then let listing = Compile.translate program prog_name verbose testmode in 
							print_string listing
						else raise(Failure("\nInvalid program.\n"))
	with 
		| InvalidArgument -> ignore (Printf.printf "InvalidArgument\n %s\n" usage)
		| NoInputFile -> ignore (Printf.printf "The second argument must be the name of an l-system file\n %s\n" usage)
