open Ast
open Str
open Lsystemstd

type var_table = {variables : Ast.vdecl list;}

type env = {
	mutable functions : func list ;
	variables : vdecl list;	
}

let equal_function_names name = function
  DFunc(f) -> f.name = name
| CFunc(f) -> f.fname = name

(*determines if the given function exists*)
let exists_function func env = 
	let name = (match func with DFunc(f) -> f.name | CFunc(f) -> f.fname) in 
		try
			let _ = List.find (equal_function_names name) env.functions in
				let e = "Duplicate function name: " ^ name in 
					raise (Failure e)
		with Not_found -> false

(*Determine if a function with given name exists*)
let exists_function_name name env = 
	try
		let _ = List.find (equal_function_names name) env.functions in 
			true (*Found a function with name like that*)
	with Not_found -> false   

(*Returns the function that has the given name*)
let get_function_name name env = 
	try
		let afunc = List.find (equal_function_names name) env.functions  in 
			afunc (*Found a function with name like that*)
	with Not_found -> raise(Failure("Function " ^ name ^ "has not yet been declared" ) )

(*Exists function when func == CFunc *)
let cexists_function func env = 
	try
		let _ = List.find (function DFunc(f) -> f.name = func.fname | CFunc(f) -> f.fname = func.fname) env.functions in 
			let e = "Duplicate function name : " ^ func.fname ^ "\n" in
				raise(Failure e) (*throw error on duplicate func.*)
	with Not_found -> false

(*this is for compute functions only*)
let cexists_formal_param func fpname =
	try
 		let _ = List.find (function fp -> let FParam(_,cn) = fp in cn = fpname) func.formals in
			true (*we're able to find a formal paramter*)
	with Not_found -> false (*no formal parameter found in the function*)

(*Determines if a formal paramter with the given name 'fpname' exits in the given function*)
let exists_formal_param func fpname =
	match func with
	| DFunc(func) -> false (*not applicable*)
	| CFunc(func) -> cexists_formal_param func fpname

(*for computing functions only*)
let cexists_variable_decl func vname =
	try
		let _ = List.find (function VDecl(_,vn,_) -> vn = vname) func.locals in
			true (*we're able to find a variable*)
	with Not_found -> false (*no variable declaration - found in the function*)

(*Determines if a variable declaration with the given name 'vname' exists in the given functioin*)
let exists_variable_decl func vname =
	match func with 
	| DFunc(func) -> false(*not applicable*)
	| CFunc(func) -> 
		try
			let _ = List.find (function VDecl(_,vn,_) -> vn = vname) func.locals in
				true (*we're able to find a variable*)
		with Not_found -> false (*no variable declaration - found in the function*)


(*this gets formal paramters for COMPUTE function*)
let get_cfparam_type func fpname =
	try
		let fparam = List.find (function FParam(_,cn) -> cn = fpname) func.formals in
			let FParam(dt,_) = fparam in 
				dt (*return the data type*)
	with Not_found -> raise (Failure ("Formal Parameter " ^ fpname ^ " should exist but was not found in compute function " ^ func.fname)) (*this shouldn't not happen*)

(*gets the variable type - only for COMPUTE functions*)									
let get_var_type func vname = 
	try
		let var = List.find (function VDecl(_,vn,_) -> vn = vname) func.locals in
			let VDecl(dt,_,_) = var in 
				dt (*return the data type*)
	with Not_found -> raise (Failure ("Variable " ^ vname ^ " should exist but was not found in compute function " ^ func.fname)) (*this shouldn't not happen*)		

(*Returns the type of a given variable name *)
let get_type func name = 
	if (cexists_variable_decl func name) (*It's a variable*)
		then get_var_type func name 
	else 
		if (cexists_formal_param func name) then
			get_cfparam_type func name
		else (*Variable has not been declared as it was not found*)
			let e = "Variable " ^ name ^ " is being used without being declared in function " ^ func.fname in
				raise (Failure e)

(*Determines if the given identiifier exists*)
let exists_id name func = (cexists_variable_decl func name) or (cexists_formal_param func name)
 
(*see if there is a function with given name "func"*)
let find_function func env = 
	try
		let _ = List.find (equal_function_names func) env.functions in 
			true (*return true on success*)
	with Not_found -> raise Not_found  

let dup_fparam func = 
    match func with
        | DFunc(func) -> let length = List.length func.formal in 
            if(length = 1) then (*must have 1 arguments*)
                let _isvalid = List.map(
                                       fun(x) -> let FParam(t,_) = x in match t with
                                            | IntType -> false
                                            | _ -> raise(Failure("Formal parameter type for draw function must be an int"))
                                        ) func.formal in false
            else
                raise(Failure("Draw function '"^ func.name ^"' must have only 1 formal parameters but it has " ^ string_of_int length ^ " params"))

        | CFunc(func) -> let isdup f = List.fold_left(
                                                      fun c x ->
                                                        let FParam(_,my_name) = f and FParam(_,curr_name) = x in
                                                            if ( c = 0 && my_name = curr_name ) then c + 1                                                                                                                                    
                                                            else 
                                                                if ( c = 1 &&  my_name = curr_name) then (*found a 2nd dup match*)
                                                                    let e = "Duplicate formal parameter in function : " ^ func.fname ^ "\n" in
                                                                    raise(Failure e) (*throw error on duplicate formal parameter.*) 
                                                                else c
                                                      ) 0 func.formals 
                            in let _ = List.map(isdup) func.formals
                                in false 

(*This check for duplicate formal parametersin COMPUTE function*)
let cdup_fparam func = 
	let isdup f = List.fold_left(
															 fun c x ->
																	let FParam(_,my_name) = f and FParam(_,curr_name) = x in
																	if ( c = 0 && my_name = curr_name ) then c + 1   																  
																	else 
																			if ( c = 1 &&  my_name = curr_name) then (*found a 2nd dup match*)
  																			let e = "Duplicate formal parameter in function : " ^ func.fname ^ "\n" in
													  					  raise(Failure e) (*throw error on duplicate formal parameter.*) 
																			else c
																			
																					 ) 0 func.formals 
	in let _ = List.map(isdup) func.formals
	in false

(*checks if there is a duplicate variable declaration for COMPUTE functions*)
let dup_vdecl func =
	match func with
	| DFunc(func) -> false
	| CFunc(func)-> 		
	let isdup var = List.fold_left(
																 fun c x ->
																		let VDecl(mdt,mn,_) = var
																		and VDecl(tdt,tn,_) = x in 
																		if ( c = 0 && (mn) = (tn)  ) then c + 1   																  
																		else 
																				if ( c = 1 &&  (mn) = (tn) ) then (*found a 2nd dup match*)
					  																			let e = "Duplicate variable declaration '"^ mn ^"' in compute function : " ^ func.fname  in
														  					  raise(Failure e) (*throw error on duplicate formal parameter.*) 
																				else c
																				
																	 ) 0 func.locals

											(*check if the given variable decl. name has already been declared in the formal paramters*)
											 in let _ = List.map( 
																											fun(x) -> List.map( 
																																					 fun(y) -> let FParam(_,formal_nm) = y 
																																										 and VDecl(_,varname,_) = x 
																																										 in if (formal_nm = varname) then 
																																														let e = "Redeclaration of formal parameter '" ^ formal_nm ^"' not allowed in function : " ^ func.fname ^"\n"
																																														in raise(Failure e)
																																											  else false 
																																						) func.formals
																								  ) func.locals
											 in 
											 let _ = List.map(isdup) func.locals (*see if we have duplicate var names*) 
											 in false 

let is_int s = 
	try ignore (int_of_string s); true
	with _ -> false
	
let is_float s = 
	try ignore (float_of_string s); true
	with _ -> false	

let is_letter s = string_match (regexp "[A-Za-z]") s 0

let is_string s = string_match (regexp "\".*\"") s 0

let is_string_bool = function "true" -> true | "false" -> true | _ -> false

(*check if variable declation is valid*)
let valid_vdecl func =
	match func with
		| DFunc(func) -> false
		| CFunc(func) -> 
			let _ = List.map (function VDecl(dt,nm,value) -> 
				let e = "Invalid variable declaration for '" ^ nm ^ "' in compute function " ^ func.fname ^ "\n" in
					let be = e ^ "The only allowed values for initializing boolean variables are 'true' and 'false.' \\n" in
						function
						  StringType  -> if is_string value then true else raise (Failure e)
						| DoubleType  -> if is_float value then true else raise (Failure e) 
						| IntType     -> if is_int value then true else raise (Failure e)
						| BooleanType -> if is_string_bool value then true else raise (Failure be)) func.locals in
							true

let rec is_num func = function
	  Literal(i) -> true
	| Float(f) -> true
	| Id(s) -> (function IntType -> true | DoubleType -> true | _ -> false) (get_type func s) 
	| Binop(e1,op,e2) -> (is_num func e1) && (is_num func e2)
	| Call(name,expr) -> raise (Failure "Direct Function calling in an arithmetic expression is not permitted") 															
	| _ -> false 

let rec get_expr_type e func = 
	match e with
		| String(s) -> StringType
    | Id(s) -> get_type func s
		| Literal(i) -> IntType
		| Float(f) -> DoubleType
		| Boolean(b) -> BooleanType
		| Binop(e1,op,e2) -> let t1 = get_expr_type e1 func  and
		                     t2 = get_expr_type e2 func  in  
												begin
												 match t1,t2 with
														| DoubleType,DoubleType -> DoubleType
														| DoubleType,IntType -> DoubleType (*Upconvert to double type*)
														| IntType,DoubleType -> DoubleType (*Upconvert to double type*)
														| IntType,IntType -> IntType
														| _,_ -> raise (Failure ("Invalid Types used in a binop expression"))
												end
		| Assign(id,expr) -> get_expr_type expr func
		| Call(fname,expr) -> DoubleType (*function calls return double*)
		| BVal(b) -> BooleanType
		| RExpr(e1,rop,e2) -> let t1 = get_expr_type e1 func and
		                      t2 = get_expr_type e2 func in
												  begin 
														match t1,t2 with
															| DoubleType,DoubleType -> BooleanType
															| DoubleType,IntType -> BooleanType 
															| IntType,DoubleType -> BooleanType 
															| IntType,IntType -> BooleanType
															| _,_ -> raise(Failure("Invalid Types used in a relational expression"))
														end
		| EExpr(e1,eop,e2) -> let t1 = get_expr_type e1 func and
		                      t2 = get_expr_type e2 func  in
													begin 
														match t1,t2 with
															| DoubleType,DoubleType -> BooleanType
															| DoubleType,IntType -> BooleanType 
															| IntType,DoubleType -> BooleanType 
															| IntType,IntType -> BooleanType
															| StringType,StringType -> BooleanType (*can do string comparisons*)
															| BooleanType,BooleanType -> BooleanType (*can compare bool values*)
															| _,_ -> raise(Failure("Invalid Types used in a equality expression"))
														end
		| BExpr(e1,bop,e2) -> let t1 = get_expr_type e1 func and
		                      t2 = get_expr_type e2 func  in
													begin
														match t1,t2 with
															| BooleanType,BooleanType -> BooleanType
															| _,_ -> raise(Failure("Invalid Types used in a boolean compound expression"))
													end
    | Bracket(e) -> get_expr_type e	func										
	  | _ -> raise(Failure("An unexpected error occured")) (*should not happen - added this to turn off compiler warnings about incomplete matching for Noexpr*)


		                      
(*Checks if the given expression is a valid  assignment / call expression*)
let is_assign_call func = function
	  Assign(_,_) -> true
	| Call(_,_) -> true
	| _ -> false

(*Makes sure that the given arguments in a function call match the function signature*)
(*fname of function being called*)
(*exprlist - list of expr in funcation call*)
(*cfucn- compute function*)
(*env - the enviroment*)
let check_types fname exprlist cfunc env =
  let func = get_function_name fname env in
	begin
	
		match func with
      | DFunc(func) -> if(List.length exprlist = 1) then
				                    let e = List.hd exprlist in 
				                    let arg_type = get_expr_type e cfunc in
														match arg_type with
															| IntType -> 0
															| _ -> raise(Failure("Draw function call "^fname ^"is not used with an int parameter in compute function " ^ cfunc.fname))
				               else
												raise(Failure("Draw function call "^fname^" not supplied with a single actual parameter of type int"))
      | CFunc(func) -> 
                      let arg_types = List.map(fun(e) -> get_expr_type e cfunc) exprlist in 
                        if((List.length arg_types) != (List.length func.formals) ) then (*number of args don't match up*)
													raise(Failure("Number of arguments in a function call don't match up in compute function " ^ func.fname))
												else
													 let check_arg c arg_type = (*c is the counter, arg_type is type of actual parameters. meant to be used in the list.foldleft *)
														     let formal_param = List.nth func.formals c in
																 let FParam(formal_type,_) = formal_param in						 
																 begin 
																	match formal_type,arg_type with 
																	| DoubleType, DoubleType -> c + 1								                
										              | DoubleType, IntType -> c + 1																		
																	| IntType, IntType -> c + 1
																	| StringType, StringType -> c+1
																	| BooleanType, BooleanType -> c+1 
																	| _,_ -> raise(Failure("Types don't match in call expression " ^ fname ^ " in the compute function " ^ cfunc.fname)) 
													      end
													in  
													 List.fold_left (check_arg) 0 arg_types											
  end




(**
Ensure that the lsystem function call is valid. Possible options are 
down - 0 params
up - 0 param
setX - 1 int parameter
setY - 1 int parameter
turn - 1 int parameter
forwar - 1 int parameter
------------------------
Arguments: 
fname - the compute/draw function in which this function call is loacted in
name - the name of function being called
exprlist - actual parameters in function call
*)

(*Tim: I'm halfway through with this*)
let valid_lfcall fname name exprlist = 
	let len = List.length exprlist in 
		match name with
		| "down" -> if len = 0 then true else raise(Failure("Down function is not called with 0 parameters in function" ^ fname))
		| "up"   -> if len = 0 then true else raise(Failure("Up function is not called with 0 parameters in function" ^ fname))
		| "forward" -> let arg = string_of_expr (List.hd exprlist) in 
			if len = 1 
				then if is_int arg or is_float arg
					then true
					else raise(Failure("Forward function not used with an integer or double parameter in function " ^ fname))
				else raise(Failure("Forward function is called with more than 1 parameter in function" ^ fname))
		| "turn" -> if len = 1 then
                    let e = List.nth exprlist 0 in 
                    let s = string_of_expr e in
                    let ok = is_int s in 
                    if(ok) then
                        true
                    else
                        raise(Failure("forward function is not used with an integer parameter in function " ^ fname))
                else
                    raise(Failure("Forward function is called with more than 1 parameters in function" ^ fname))
        | "setX" -> if((List.length exprlist) = 1) then
                    let e = List.nth exprlist 0 in 
                    let s = string_of_expr e in
                    let ok = is_int s in 
                    if(ok) then
                        true
                    else
                        raise(Failure("setX function is not used with an integer parameter in function " ^ fname))
                else
                    raise(Failure("setX function is called with more than 1 parameters in function" ^ fname))
                                        
        | "setY" -> if((List.length exprlist) = 1) then
                    let e = List.nth exprlist 0 in 
                    let s = string_of_expr e in
                    let ok = is_int s in 
                    if(ok) then
                        true
                    else
                        raise(Failure("setY function is not used with an integer parameter in function " ^ fname))
                else
                    raise(Failure("setY function is called with more than 1 parameters in function" ^ fname))
        | _ -> raise(Failure("Invalid call "^name ^" in  function " ^ fname))
        



(*Makes sure that the stdlib call is a valid one. Should be called after making sure that the said library function exists. *)
(*At the moment we allow std turtle functions up,down etc to be called *)
(*fname - the compute/draw function in which this function call is loacted in*)
(*name - the name of function being called*)
(*exprlist - actual parameters in function call*)

(*what??*)
let valid_fstdcall fname name func exprlist = 
	match name with
		| "print" -> if List.length exprlist = 1 then
			let e = List.hd exprlist in
				let t = get_expr_type e func in
					match t with
						| IntType -> true
						| StringType -> true
						| DoubleType -> true
						| BooleanType -> true
			else
				raise(Failure("Print called with more than 1 parameter in function " ^ fname))  
		| "up" -> valid_lfcall fname name exprlist  
		| "down" -> valid_lfcall fname name exprlist
		| "forward" -> valid_lfcall fname name exprlist
		| "turn" -> valid_lfcall fname name exprlist
		| "setX" -> valid_lfcall fname name exprlist
		| "setY" -> valid_lfcall fname name exprlist
		|  _ -> raise(Failure("Invalid call "^name ^" in  function " ^ fname))
		
			  

let rec valid_expr (func : Ast.func_decl) expr env = 
	
	match expr with
		| Literal(i) -> true 
		| Float(f) ->  true
		| Boolean(b) -> true  
		| String(s) -> true
		| Id(s) -> if(exists_id s func) then true else raise( Failure ("Undeclared identifier " ^ s ^ " is used" ))
		| Binop(e1,op,e2) -> let r1 = is_num func e1 
												 and r2 = is_num func e2 in
												 r1 && r2
		| Assign(id, e1) -> if(exists_id id func) then
														let dt = get_type func id and 
														_ = valid_expr func e1 env and 
														exprtype = get_expr_type e1 func 
														in match dt,exprtype with
															| StringType,StringType -> true
															| IntType,IntType -> true
															| DoubleType,DoubleType -> true
															| DoubleType,IntType -> true (*allow int to double conversion*)
															| BooleanType,BooleanType -> true
															| IntType,DoubleType -> raise(Failure ("Cannot assign a double to an int"))
															| _,_ -> raise(Failure ("DataTypes do not match up in an assignment expression to variable " ^ id))
														
											  else
													raise( Failure ("Undeclared identifier " ^ id ^ " is used" ))
													
     													
		| Call(fname, exprlist) ->  if(exists_function_name fname env) then
															     let _has_valid_exprs = List.map(fun(e) -> valid_expr func e env) exprlist in 
																	 let _checktypes = check_types fname exprlist func env (*check that the types match up otherwise throws an error *) 
																	 in
																	 true 
			                         else 
															     (if List.mem fname Lsystemstd.func_names then (*It's a standard library function call*)
																    valid_fstdcall func.fname fname func exprlist (*std turtle functions can be called*)
															     else
															     	raise( Failure ("Undefined function : " ^ fname ^ " is used" )) 
																    )	
		| BVal(b) -> true
		| RExpr(e1,rop,e2) -> let t1 = get_expr_type e1 func and
		                      t2 = get_expr_type e2 func in
													begin
														match t1,t2 with
															| DoubleType,DoubleType -> true
															| DoubleType,IntType -> true
															| IntType,IntType -> true
															| IntType,DoubleType -> true
															| _,_ -> raise(Failure("Invalid Types used in a relational expression"))
													end
		| EExpr(e1,eop,e2) -> let t1 = get_expr_type e1 func and
		                      t2 = get_expr_type e2 func in
													begin
														match t1,t2 with
															| DoubleType,DoubleType -> true
															| DoubleType,IntType -> true
															| IntType,IntType -> true
															| IntType,DoubleType -> true
															| StringType,StringType -> true
															| BooleanType,BooleanType -> true
															| _,_ -> raise(Failure("Invalid Types used in an equality expression"))
													end
		| BExpr(e1,bop,e2) -> let t1 = get_expr_type e1 func and
		                      t2 = get_expr_type e2 func in
													begin
														match t1,t2 with
															| BooleanType,BooleanType -> true
															| _,_ -> raise(Failure("Invalid Types used in a boolean compound expression"))
													end
    | Bracket(e) -> valid_expr func e env													
	  | _ -> false(*should not happen - added this to turn off compiler warnings about incomplete matching for Noexpr*)


(*Returns alphabet list from the draw function*)
let get_alphabet_list func =
	let LSystem(alphabet,lambda,rlist) = func.rules in
		let Alphabet(alphabet_list) = alphabet in
			alphabet_list

(*Check to make sure that alphabet has no repeating letters*)
let valid_alphabet alphabet func =
            let Alphabet(alphabet_list) = alphabet in  
        let isdup letter = List.fold_left(
                                               fun c curr_letter -> 
                                                                                                        if ( c = 0 && letter = curr_letter) then c + 1                                                                    
                                else 
                                   if ( c = 1 &&  letter = curr_letter) then (*found a 2nd duplicate match*)
                                      let e = "Duplicate alphabet letters '" ^ letter ^ "' in function : " ^ func.name ^ "\n" in
                                      raise(Failure e)  
                                   else c
                                            ) 0 alphabet_list
                        in 
                        let _ensure_no_dups  = List.map(isdup) alphabet_list in
                        let valid_letters = List.for_all (is_letter) alphabet_list in
                        match valid_letters with
                                | true -> true
                                | false -> raise(Failure("Invalid letters used in alphabet of drawing function " ^ func.name)) 
	
(*Check if given symbol exists in alphabet*)
let exists_in_alphabet letter alphabet_list  =
	try
	   let _ = List.find (fun(x) -> x = letter ) alphabet_list in
		 true
	with Not_found -> false

(*check if given symbol is in the standary library symbol of 'l r f'*)
let is_std_symbol s =
	try
       let _ = List.find (fun(x) -> x = s ) Lsystemstd.std_symbols in
         true
    with Not_found -> false	
			
(*Check if the given letter exists in alphabet or is part of the 'l r f' standard library symbols*)	
let valid_symbol letter func = 
	let in_alphabet = exists_in_alphabet letter (get_alphabet_list func) and
	is_std_symbol = is_std_symbol letter in
	match in_alphabet,is_std_symbol with
		| true,_ -> true
		| _,true -> true
		| false,false -> false
		

(*checks to see if the lfunc call is std call : up/down/turn/*)
let valid_lstdcall name =
	try
		let _ =List.find(fun(x) -> x = name) Lsystemstd.std_lfunc
		in true
	with Not_found -> false 


let valid_rule rule func =
    match rule with
        | Lambda(string_list) -> let ok = List.for_all(fun(x) -> valid_symbol x func) string_list in
				                         if(ok) then
																	true
																else
																	raise(Failure("Lambda rule has an invalid character that has not been declared in the alphabet"))
        | ERule(name, string_list) -> if(valid_symbol name func) then
																					let ok = List.for_all(fun(x) -> valid_symbol x func) string_list in  
																					if(ok) then
																						true
																				  else 
																						raise(Failure("ERule '"^ name ^"' has an invalid character that has not been declared in the alphabet"))
					                           else
																			  raise(Failure("ERule symbol '"^ name ^"' is not in the alphabet"))
			  | FRule(name, fname,string_list) -> if(valid_symbol name func) then
					                                       let _valid_call = valid_lfcall func.name fname string_list (*ensure function is valid ; throw an error if not*)
																								 in true
					                                  else
																							raise(Failure("FRule symbol '" ^ name ^ "' is not in the alphabet"))
				| EmptyFRule(name) -> if (valid_symbol name func) then
					                       true
					                   else 
															   raise(Failure("Empty FRule symbol '"^ name ^"' is not in the alphabet"))


(*validates the lsystem in a draw funciton*)
let validate_lsystem func env = 
	let LSystem(alphabet,lambda,rlist) = func.rules in
	let _validate_alphabet = valid_alphabet alphabet func in
	let _valid_lambda = valid_rule lambda func in
	let _valid_rules = List.map(fun(x) -> valid_rule x func ) rlist
	in
	true

(*Checks the body of a compute function *)
let valid_body func  env =
	match func with
		| DFunc(func) -> validate_lsystem func env
		| CFunc(func) ->  	
						let rec check_stmt st =	
														begin													
															match st with
																(*the 'block' will only occur in if and while condition loop. *)
																| Block(st_list) -> let _ = List.map(fun(x) -> check_stmt x) st_list (*Check statements in the block. Err will be thrown for an invalid stmt*) 
																							      in true 
																| Expr(st) -> let vldexpr = valid_expr func st env and (*make sure the expression is valid expression*)
																							assign_call  = is_assign_call func st in
																							begin 
																								match vldexpr,assign_call with (*The expression MUST be valid and also an assignment/call expression. Can't have '1;' as a stmt expr alone *)
																									| true,true -> true
																									| true,false -> raise(Failure ("Invalid expression (No var assignment) in function " ^func.fname ^ "\n"))
																									| false,_ -> raise(Failure ("Invalid assignment expression in function " ^func.fname ^ "\n"))
																							end 																						 
																| Return(st) -> let ret = get_expr_type st func in 
																													begin 
																														match ret with
																															| DoubleType -> true
																															| IntType -> true
																															| _ -> raise(Failure("return type is not double in compute function " ^ func.fname ^ ". It is of type :" ^ (string_of_dt ret)))
																													end
																| If(predicate,stmt1,stmt2) -> let pred_type = get_expr_type predicate func and 
																												     ve1 = check_stmt stmt1 and
																												     ve2 = check_stmt stmt2 in
																												 let _vpred = (*Check predicate*)  
																																		begin
																																		 match pred_type with
																																			| BooleanType -> true
																																			| _ -> raise(Failure("predicate expression must be a valid boolean expression that evaluates to true/false"))
																																		end
																												 in
																												 begin match ve1,ve2 with
																													| true, true -> true
																													| _ , _ -> raise(Failure("Invalid expression used in if statement in compute function " ^ func.fname ^ "\n"))
																												 end																                      
																| For(_,_,_,_) -> let e = "For loop are not allowed in function" ^ func.fname ^ "\n" in
																				 							raise (Failure e) (*don't have to worrty about this case b'se parser will give parse error for 'for loops'.*)
																| While(pred,stmts) -> let isvalid = check_stmt stmts in (*need to test*)
                                                                    begin 
																																		match isvalid with
																																			| true -> true
																																			| false ->  raise (Failure ("Invalid statement found inside while loop in compute function " ^func.fname ^"\n"))
                                                                    end
														end 
										 in 
										let _ = List.map(check_stmt) func.body 
										in true 


(*Check a Compute Function. *)
(* The type of function 'f' passed however should be of type *)
(* Ast.func This is so that we can easily add functions to environment*)
(* and avoid having fields for Cfunctions and Dfunctions separately. *)
let check_cfunc f env = 
	let dup_fname = exists_function f env in
		let dup_formals = dup_fparam f in 
			let vlocals = (not (dup_vdecl f)) && (valid_vdecl f) (*make sure that we've no dup variable names, and data types match up*) in
				let vbody = valid_body f env in 
					let _ = env.functions <- f :: env.functions (*add function name to environment *) in 
						(not dup_fname) && (not dup_formals) && vlocals && vbody

let check_dfunc f env = 
	let dup_fname = exists_function f env in
		let dup_formals = dup_fparam f in
			let vbody = valid_body f env in
				let _ = env.functions <- (f) :: env.functions (*add function name to environment *) in
					(not dup_fname) && (not dup_formals) && vbody

let valid_func env = function
	  CFunc(f) -> let afunc = CFunc(f) in check_cfunc afunc env   
	| DFunc(f) -> let afunc = DFunc(f) in check_dfunc afunc env  

(*Checks to make sure that the main function exists and is a compute function*)
let exists_main env =
	if exists_function_name "main" env then
		ignore ((function CFunc(_) -> true | DFunc(_) -> raise(Failure("main function must be a compute function."))) 
			(get_function_name "main" env))

let check_program flist =
	let (environment : env) = { functions = [] ; variables = [] } in
		let _dovalidation = List.map ( fun(f) -> valid_func environment f) flist in (*Do the semantic analysis*)
			let _mainexists = exists_main environment (*ensure that a main function exists*) in
				let _ = print_endline "\nSemantic Analysis Completed Successfully.\nCompiling...\n" in 
					true 
