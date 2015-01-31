open Ast

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding
and environment = value ref Environment.environment

(* Helper function for un-nesting datum into a datum list
 * Requires: takes in a datum that is of type Cons(a, Cons (b...))), Nil)
 * Returns: a list of datums *)
let rec dat_lst (inp : datum) (lst : datum list) : datum list =
  match inp with
  | Cons(a, Nil) -> List.rev (a::lst)
  | Cons(a, b) -> dat_lst b (a::lst)
  | _ -> failwith "input should be Cons ending with Nil"

(*Helper function for converting datum to variable
 *Requires: raises an error if datum is not a variable
 *Returns: datum converted into a variable*)
let dat_to_var (data: datum) : variable =
  match data with
  | Atom (Identifier id) -> Identifier.variable_of_identifier id
  | _ -> failwith "Invalid variable"

(* Parses a datum into an expression. 
 * Requires: takes in a valid datum
 * Returns: an expression*)
let rec read_expression (input : datum) : expression =
  match input with
  | Atom (Identifier id) when Identifier.is_valid_variable id -> 
    ExprVariable (Identifier.variable_of_identifier (id))
  | Atom (Identifier id) -> failwith "Invalid variable form"
  | Atom (Boolean v) -> ExprSelfEvaluating (SEBoolean v)
  | Atom (Integer c) -> ExprSelfEvaluating (SEInteger c)
  | Nil -> failwith "Unknown expression form"
  | Cons (a,b) -> match a with
                 | Atom (Boolean v) -> failwith ""
                 | Atom (Integer c) -> failwith ""
                 | Atom (Identifier id) when (Identifier.is_valid_variable id)-> 
                   ExprProcCall(read_expression a, List.map read_expression (dat_lst b []))
                 | Atom (Identifier id) ->
                    begin
                      match Identifier.string_of_identifier (id), b with
                     |"quote", Cons(dat, Nil) -> ExprQuote dat
                     |"if", Cons(e1, Cons(e2, Cons(e3, Nil))) -> 
                       ExprIf (read_expression e1, read_expression e2, read_expression e3)
                     |"lambda", Cons(var, exp) -> 
                      ExprLambda (List.map dat_to_var (dat_lst var []),
                                  List.map read_expression (dat_lst exp []))
                     |"set!", Cons(var, Cons(exp, Nil)) -> ExprAssignment 
                      (dat_to_var var, read_expression exp)
                     |"let", Cons(bindings, exp) ->
                       ExprLet (List.map dat_to_bind (dat_lst bindings []),
                                List.map read_expression (dat_lst exp []))
                     |"let*", Cons(bindings, exp) ->
                       ExprLetStar (List.map dat_to_bind (dat_lst bindings []),
                                    List.map read_expression (dat_lst exp []))
                     |"letrec", Cons(bindings, exp) ->
                       ExprLetRec (List.map dat_to_bind (dat_lst bindings []),
                                   List.map read_expression (dat_lst exp []))
                     |"define", _ -> failwith "define not allowed as an expression, only at the toplevel"
                     |_ , _ -> failwith "Invalid datum" 
                    end
                 | Cons _ -> failwith "Unknown expression form"
                 | Nil -> failwith "Unknown expression form"

(* Helper function for creating bindings
 * Requires: a valid datum, arises an error if datum is 
 *   not in the format of Cons(a, Cons (b, Nil))
 * Returns: a let_binding of the datum containing variable and expression *)
and dat_to_bind (data: datum) : let_binding = 
      match data with
      | Cons(a, Cons(b,Nil)) -> (dat_to_var a, read_expression b)
      | _ -> failwith "Invalid_let_binding"

(* Parses a datum into a toplevel input. 
 * Requires: raises an error if toplevel definition is in the incorrect format
 * Returns: a toplevel after parsing the datum*)
let read_toplevel (input : datum) : toplevel =
  match input with
  | Cons(Atom (Identifier id), b) ->
      begin
        match Identifier.string_of_identifier (id), b with
        |"define", Cons(var, Cons (ex, Nil)) -> ToplevelDefinition (dat_to_var var, read_expression ex)
        |"define", _ -> failwith "Invalid define form"
        | _, _ -> ToplevelExpression (read_expression input)
      end
  | _ -> ToplevelExpression (read_expression input)

(* Requires: takes in a unit
 * Returns: initial environment with any built-in
 * bound variables. *)
let rec initial_environment () : environment =

  (* Helper function that converts a string into a variable
   * Requires: a string s
   * Returns: a variable with the content of string s*)
  let var_of_str (s:string): variable = 
    Identifier.variable_of_identifier (Identifier.identifier_of_string s) in

  (* Helper function for built_in car which
   * Requires: takes in a single cons-cell datum; raises an
   *   error if the procedure is not called with a single
   *   cons-cell datum
   * Returns: the first element of the cons-cell *)
  let help_car (lst: value list) (en: environment): value = 
    match lst with 
    | ValDatum (Cons (d1, d2))::[] -> ValDatum (d1) 
    | _ -> failwith "Invalid arguments to car" in 

  (* Helper function for built_in cdr which
   * Requires: takes in a single cons-cell datum; raises an
   *   error if the procedure is not called with a single
   *   cons-cell datum
   * Returns: the second element of the cons-cell *)
  let help_cdr (lst: value list) (en: environment): value =
    match lst with 
    | ValDatum (Cons (d1, d2))::[] -> ValDatum (d2) 
    | _ -> failwith "Invalid arguments to car" in 

  (* Helper function for built_in cons function which
   * Requires: takes in two data, raises an error if the 
   *   procedure is not called with exactly two arguments.
   * Returns: cons-cell whose car is the first argument 
   *   and cdr is the second argument.*)
  let help_cons (lst: value list) (en: environment): value = 
    match lst with 
    | [] -> failwith "Invalid arguments to cons"
    | h::[] -> failwith "Invalid arguments to cons"
    | h1::h2::[] -> 
      begin
        match (h1, h2) with 
        | (ValDatum d1, ValDatum d2) -> ValDatum (Cons (d1, d2))
        | _ -> failwith "The values must be of type ValDatum"
      end 
    | _ -> failwith "Invalid arguments to cons" in 
  
  (* Helper function for built_in + which
   * Requires: a list of integers; if the procedures
   *   are called with any non-integer arguments, raises an error
   * Returns: OCaml sum of the integer values*)      
  let help_plus (lst: value list) (en: environment): value = 
    let rec sum (vlist: value list): int = 
      match vlist with 
      | [] -> 0
      | ValDatum (Atom (Integer x))::tl -> x + (sum tl) 
      | _ -> failwith "Invalid arguments to plus" in 

    match lst with
    | [] -> failwith "Invalid number of arguments to plus"
    | h::t -> ValDatum (Atom (Integer (sum lst))) in

  (* Helper function for built_in * which 
   * Requires: a list of integers; if the procedures
   *   are called with any non-integer arguments, raises an error
   * Returns: OCaml product of the integer values*)    
  let help_mult (lst: value list) (en: environment): value = 
    let rec mult (vlist: value list): int = 
      match vlist with 
      | [] -> 1
      | ValDatum (Atom (Integer x))::tl -> x * (mult tl)
      | _ -> failwith "Invalid arguments to multiply" in

    match lst with 
    | [] -> failwith "Invalid arguments to multiply"
    | h::t -> ValDatum (Atom (Integer (mult lst))) in 

  (* Helper function for built-in function equal? which
   * Requires: takes in two arguments; raises an error
   *   if the procedure is not called with exactly two arguments;
   *   raises an error if equal? is called on procedures
   * Returns: whether its first argument is structurally equal
   * to its second argument.*)
  let help_equal (lst: value list) (en: environment) : value =
    match lst with 
    | (ValDatum d1)::(ValDatum d2)::[] -> 
      if d1 = d2 then ValDatum (Atom (Boolean true)) else ValDatum (Atom (Boolean false))
    | _ -> failwith "Invalid arguments to equal" in 
  
  (* Helper function for built-in procedure eval
   * Requires: takes in a single datum
   * Returns: evaluated value of the datum*)
  let help_eval (lst: value list) (en: environment) : value =
    match lst with
    | [] -> failwith "Invalid arguments to eval"
    | (ValDatum d)::[] -> eval (read_expression d) en  
    | _ -> failwith "Invalid arguments to eval" in 

  let initial = Environment.empty_environment in
  let course = 
    Environment.add_binding initial (var_of_str "course", 
    ref (ValDatum (Atom (Integer 3110)))) in 
  let car = Environment.add_binding course (var_of_str "car",
    ref (ValProcedure (ProcBuiltin help_car))) in 
  let cdr = Environment.add_binding car (var_of_str "cdr", 
    ref (ValProcedure (ProcBuiltin help_cdr))) in
  let cons = Environment.add_binding cdr (var_of_str "cons", 
    ref (ValProcedure (ProcBuiltin help_cons))) in 
  let plus = Environment.add_binding cons (var_of_str "+", 
    ref (ValProcedure (ProcBuiltin help_plus))) in 
  let mult = Environment.add_binding plus (var_of_str "*", 
    ref (ValProcedure (ProcBuiltin help_mult))) in 
  let equal = Environment.add_binding mult (var_of_str "equal?", 
    ref (ValProcedure (ProcBuiltin help_equal))) in 
  let eval = Environment.add_binding equal (var_of_str "eval", 
    ref (ValProcedure (ProcBuiltin help_eval))) in 
  eval

(* Evaluates an expression down to a value in a given environment.
 * Requires: a valid expression and environment. otherwise, 
 * raises an error.
 * Returns: an evaluated value of the expresion in the given
 * environment *)
and eval (expression : expression) (env : environment) : value =

  (*Helper function for evaluating ExprSelfEvaluating*)
  let help_ExprSelfEvaluating (x: self_evaluating): value = 
    match x with 
    | SEBoolean b -> ValDatum (Atom (Boolean b))
    | SEInteger i -> ValDatum (Atom (Integer i)) in

  (*Helper function for evaluating ExprVariable
   *Raises a failure if variable is not bound in the environment*)
  let help_ExprVariable (x: variable): value = 
    try let d = Environment.get_binding env x in !d with 
      Not_found -> failwith "Variable is not bound in the environment" in 
  
  (* Helper function for evaluating ExprLambda
   * Raises an error if the variables do not all have distinct names.*)
  let help_ExprLambda (vlist, elist): value = 
    let rec has_dup (lst: 'a list) : bool =
      match lst with
        [] -> false
      | [_] -> false
      | h::h1::t -> let first = h in (List.fold_left (fun a e -> (e = first) || a) 
                    false (h1::t)) || has_dup (h1::t) in

    if has_dup vlist then failwith "Variables must have distinct names"
    else ValProcedure (ProcLambda (vlist, env, elist)) in 

  (*Helper function for evaluating ExprProcCall *)
  let help_ExprProcCall (ex, arg): value = 
      match eval ex env with  
      | ValProcedure pr ->  
        begin 
          match pr with 
          | ProcBuiltin built -> 
            begin 
              let rec create_val_list (elist: expression list): value list = 
                List.rev (List.fold_left (fun a e0 -> (eval e0 env)::a) [] elist) in 
              built (create_val_list arg) env
            end
          | ProcLambda (vlst, local_env, exlst) -> (*user defined procedure*)
            begin 
              let bind_var (local: environment) (var1:variable) (ex1:expression) = 
                Environment.add_binding local (var1, ref (eval ex1 env)) in 
              try 
                begin 
                  let new_env = List.fold_left2 bind_var local_env vlst arg in
                  List.fold_left (fun a e0 -> eval e0 new_env) (ValDatum Nil) exlst
                end 
              with Invalid_argument _ -> failwith "wrong number of arguments in procedure call"
            end 
        end
      | ValDatum d -> failwith "The first expression has type ValDatum but should have type ValProcedure" in    
  
  (*Helper function for evaluating ExprIf*)
  let help_ExprIf (ex0, ex1, ex2): value = 
    if eval ex0 env = ValDatum (Atom (Boolean false)) then (eval ex2 env)
    else eval ex1 env in 

  (*Helper function for evaluating ExprAssignment
  * Raises an error if identifier is not bound in the environment*)
  let help_ExprAssignment (v, e): value = 
    if Environment.is_bound env v then
      ((Environment.get_binding env v):= eval e env; ValDatum Nil)
    else failwith "identifier is not bound in this environment" in 

  (*Helper function for evaluating ExprLet*)
  let help_ExprLet (lblist, elist): value = 
    let localenv = 
      List.fold_left (fun a (var, ex) -> 
      Environment.add_binding a (var, ref (eval ex env))) Environment.empty_environment lblist in 
    let expr_env = Environment.combine_environments localenv env in 
    List.fold_left (fun a e -> eval e expr_env) (ValDatum Nil) elist in 

  (*Helper function for evaluating ExprLetStar
    To evaluate a let-binding, the binding’s expression is 
    evaluated in the current environment, then the environment 
    is extended to bind the variable to the result. Thus the 
    second binding is done in an environment where the first 
    is visible, and so forth. Finally,the body expressions of 
    the let-star are evaluated in left-to-right order, starting 
    with the environment containing all the let-bindings.*)
  let help_ExprLetStar (lblist, elist): value = 
    let localenv = 
      List.fold_left (fun extended (var, ex) ->
      Environment.add_binding extended (var, ref (eval ex extended))) env lblist in
    let expr_env = Environment.combine_environments localenv env in 
    List.fold_left (fun a e -> eval e expr_env) (ValDatum Nil) elist in

  (*Helper function for evaluating ExprLetRec
   Every binding is evaluated in an environment where all the other 
   bindings are also visible, thus enabling definition of mutually 
   recursive procedures. It must be possible to evaluate
   each binding expression without reading or writing the value of a variable in 
   any of the other binding expressions. If not, raises a failure.*)
  let help_ExprLetRec (lblist, elist): value = 
    let dummy_env = 
      List.fold_left (fun a (var, ex) -> 
      Environment.add_binding a (var, ref (ValProcedure (ProcLambda ([], env, []))))) 
      env lblist in 
    let update_env = List.fold_left (fun a (var, ex) -> 
      Environment.get_binding a var := (eval ex dummy_env); a) dummy_env lblist in 
    List.fold_left (fun a e -> eval e update_env) (ValDatum Nil) elist in 
    
  match expression with
  | ExprSelfEvaluating ex     -> help_ExprSelfEvaluating ex
  | ExprVariable ex           -> help_ExprVariable ex
  | ExprQuote d               -> ValDatum (d)   
  | ExprLambda (vlst, elst)   -> help_ExprLambda (vlst, elst) 
  | ExprProcCall (ex, elst)   -> help_ExprProcCall (ex, elst)
  | ExprIf (b, e1, e2)        -> help_ExprIf (b, e1, e2)
  | ExprAssignment (var, ex)  -> help_ExprAssignment (var, ex)  
  | ExprLet (blst, elst)      -> help_ExprLet (blst, elst)
  | ExprLetStar (blst, elst)  -> help_ExprLetStar (blst, elst)
  | ExprLetRec (blst, elst)   -> help_ExprLetRec (blst, elst)

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. 
 * Require: a valid toplevel and environment. Otherwise, raises an error
 * Returns: tuple of ValDatum Nil and an updated environment
 * with the new binding *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (var, expression) ->
      if Environment.is_bound env var then 
        begin
          let new_val = eval expression env in 
          Environment.get_binding env var:= new_val; 
          (ValDatum Nil, env)
        end 
      else 
        begin 
         let dummy_env = Environment.add_binding env (var, ref (ValDatum Nil)) in 
         let new_val = eval expression dummy_env in 
         Environment.get_binding dummy_env var:= new_val;
         (ValDatum Nil, dummy_env)
       end 

let rec string_of_value value =
  let rec string_of_datum datum =
    match datum with
    | Atom (Boolean b) -> if b then "#t" else "#f"
    | Atom (Integer n) -> string_of_int n
    | Atom (Identifier id) -> Identifier.string_of_identifier id
    | Nil -> "()"
    | Cons (car, cdr) -> string_of_cons car cdr

  and string_of_cons car cdr =
    let rec strings_of_cons cdr =
      match cdr with
      | Nil -> []
      | Cons (car, cdr) -> (string_of_datum car) :: (strings_of_cons cdr)
      | _ -> ["."; string_of_datum cdr;] in
    let string_list = (string_of_datum car) :: (strings_of_cons cdr) in
    "(" ^ (String.concat " " string_list) ^ ")" in
  
  match value with
  | ValDatum (datum) -> string_of_datum datum
  | ValProcedure (ProcBuiltin p) -> "#<builtin>"
  | ValProcedure (ProcLambda (_, _, _)) -> "#<lambda>"
