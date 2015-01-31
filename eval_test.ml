open Eval
open Ast
open Environment
open Identifier
open Assertions

(*Helper functions for type conversion*)
let var_of_str (s:string): variable = 
  Identifier.variable_of_identifier (Identifier.identifier_of_string s)

let dat_to_var (data: datum) : variable =
  match data with
  | Atom (Identifier id) -> Identifier.variable_of_identifier id
  | _ -> failwith "Invalid variable"
                   
(*IDENTIFIERS*)
let a = Atom (Identifier (identifier_of_string "a"))
let x = Atom (Identifier (identifier_of_string "x"))
let y = Atom (Identifier (identifier_of_string "y"))
let z = Atom (Identifier (identifier_of_string "z"))
let f = Atom (Identifier (identifier_of_string "f"))
let var1 = Atom (Identifier (identifier_of_string "hello"))
let var2 = Atom (Identifier (identifier_of_string "list->vector"))
let course = Atom (Identifier (identifier_of_string "course"))
let id = Atom (Identifier (identifier_of_string "id"))
let factorial = Atom (Identifier (identifier_of_string "factorial"))
let equal = Atom (Identifier (identifier_of_string "equal?"))
let even = Atom (Identifier (identifier_of_string "even?"))
let odd = Atom (Identifier (identifier_of_string "odd?"))
(*OPERATION IDENTIFIERS*)
let plus_sign = Atom (Identifier (identifier_of_string "+"))
let mult_sign = Atom (Identifier (identifier_of_string "*"))
let minus_sign = Atom (Identifier (identifier_of_string "-"))

(*BOOLEAN*)
let true1 = Atom (Boolean true)
let false1 = Atom (Boolean false)

(*INTEGERS*)
let zero = Atom (Integer 0)
let one = Atom (Integer 1)
let two = Atom (Integer 2)
let three = Atom (Integer 3) 
let five = Atom (Integer 5)
let hundred = Atom (Integer 100)
let thousand = Atom(Integer 1000)
let neg_one = Atom (Integer (-1))
let neg_two = Atom (Integer (-2))

(*KEYWORDS*)
let kw_if = Atom (Identifier (identifier_of_string "if"))
let kw_quote =  Atom (Identifier (identifier_of_string "quote"))
let kw_set =  Atom (Identifier (identifier_of_string "set!"))
let kw_let =  Atom (Identifier (identifier_of_string "let"))
let kw_letstar = Atom (Identifier (identifier_of_string "let*"))
let kw_letrec = Atom (Identifier (identifier_of_string "letrec"))
let kw_lambda =  Atom (Identifier (identifier_of_string "lambda"))
let kw_define = Atom (Identifier (identifier_of_string "define"))

(*DATUM*)
let abc = Cons (x, Cons (y, Cons (z, Nil)))
let plus = (Cons(plus_sign, Cons (x, Cons (y, Nil))))
let def1 = Cons (kw_define, (Cons (x, Cons (hundred, Nil))))
let def2 = Cons (kw_define, (Cons (x, Cons (course, Nil))))
let one_plus_two = Cons (plus_sign, Cons (one, Cons (two, Nil)))
let one_minus_two = Cons (plus_sign, Cons (one, Cons (neg_two, Nil)))
let one_plus_two_three = Cons (plus_sign, Cons (one, Cons (two, Cons (three, Nil))))
let x_plus_x = Cons (plus_sign, Cons (x, Cons (x, Nil)))
let x_plus_y = Cons (plus_sign, Cons (x, Cons (y, Nil)))
let quote1 = Cons (kw_quote, Cons (x, Nil))
let quote2 = Cons (kw_quote, Cons (one, Nil))
let quote3 = Cons (kw_quote, Cons (one_plus_two, Nil))
let if1 = Cons (kw_if, Cons (true1, Cons (one, Cons (two, Nil))))
let if2 = Cons (kw_if, Cons (false1, Cons (one, Cons (two, Nil))))
let if3 = Cons (kw_if, Cons (one, Cons (one, Cons (two, Nil))))

let lambda1 = Cons(kw_lambda, Cons(Cons(x, Nil), Cons(x, Nil)))
let set1 = Cons(kw_set, Cons(x, Cons(three, Nil)))

let define1 = Cons(kw_define, Cons(id, Cons( Cons(kw_lambda, Cons(Cons(x, Nil),
              Cons(x, Nil))) , Nil)))
let define2 = Cons(kw_define, Cons(f, Cons( Cons(kw_lambda, Cons (Cons(x, 
              Cons(y, Nil)) , Cons(x_plus_y, Nil ))) , Nil)))
let define3 = Cons(kw_define, Cons(minus_sign, Cons(Cons( kw_lambda,(Cons(
              Cons(x, Cons(y, Nil)), Cons(Cons(plus_sign, Cons(x, Cons(
              Cons(mult_sign, Cons(y, Cons(neg_one, Nil))), Nil))), Nil)))), Nil)))
let define4 = Cons(kw_define, Cons(a, Cons(one, Nil)))
let define5 = Cons(kw_define, Cons(f, Cons(Cons(kw_lambda, Cons(Cons(x, Nil), 
              Cons(Cons(plus_sign, Cons(a, Cons(x, Nil))), Nil))), Nil)))
let define6 = Cons(kw_define, Cons(a, Cons(two, Nil)))
let define7 = Cons(kw_define, Cons(x, Cons (course, Nil)))

let let1 = Cons (kw_let, Cons (Cons(Cons (x, Cons (zero, Nil)), Cons (Cons (y, 
           Cons (one, Nil)), Nil)), Cons (x_plus_y, Nil)))
let letstar1 = Cons (kw_letstar, Cons (Cons (Cons (x, Cons (zero, Nil)), Cons 
               (Cons (y, Cons (Cons (plus_sign, Cons (one, Cons (x, Nil))), 
               Nil)), Nil)), Cons (x_plus_y, Nil)))
let letrec1 = Cons (kw_letrec, Cons (Cons (Cons (factorial, Cons (Cons 
              (kw_lambda, Cons (Cons (x, Nil), Cons (Cons (kw_if, Cons (Cons 
              (equal, Cons (x, Cons (zero, Nil))), Cons (one, Cons (Cons (mult_sign, 
              Cons (x, Cons (Cons (factorial, Cons (Cons (plus_sign, Cons (x, 
              Cons (neg_one, Nil))), Nil)), Nil))), Nil)))), Nil))), Nil)), 
              Nil), Cons (Cons (factorial, Cons (five, Nil)), Nil)))

let letrec2 = Cons(kw_letrec, Cons( Cons(  Cons(even, Cons( 
	          Cons(kw_lambda, Cons( Cons(x, Nil) , Cons(Cons(kw_if, 
	          Cons( Cons(equal, Cons(x, Cons(zero, Nil))), Cons(true1, Cons(
	          Cons(odd, Cons(Cons(plus_sign, Cons(x, Cons(neg_one, Nil))), Nil)), 
	          Nil)))), Nil))), Nil)), Cons( Cons(odd, Cons( Cons(kw_lambda, Cons( 
	          Cons(x, Nil), Cons(Cons(kw_if, Cons( Cons(equal, 
	          Cons(x, Cons(zero, Nil))), Cons(false1, Cons(Cons(even, 
	          Cons(Cons(plus_sign, Cons(x, Cons(neg_one, Nil))), Nil)), Nil)))),
	          Nil))), Nil)), Nil)), Cons(Cons(even, Cons(five, Nil)), Nil)))


(***************************TEST CASES FOR READ_EXPRESSION******************************)

(*Test cases for reading Nil*)
TEST_UNIT "read_expression_nil1" =
           assert_raises (Some (Failure "Unknown expression form")) read_expression Nil
(*Test cases for reading atom*)
TEST_UNIT "read_expression_atom_test1" = 
           assert_raises (Some (Failure "")) read_expression (Cons (true1, Nil))
TEST_UNIT "read_expression_atom_test2" = 
           assert_raises (Some (Failure "")) read_expression (Cons (one, Nil))
TEST_UNIT "read_atom_id_test1" = 
           assert_true (read_expression var1 = 
           ExprVariable (variable_of_identifier (identifier_of_string "hello")))
TEST_UNIT "read_atom_id_test2" = 
           assert_true (read_expression var2 = ExprVariable 
           (variable_of_identifier (identifier_of_string "list->vector")))
TEST_UNIT "read_atom_id_test3" = 
           assert_raises (Some (Failure "Invalid variable form")) 
           read_expression kw_if
TEST_UNIT "read_atom_bool_test1" = 
           assert_true (read_expression true1 = 
           ExprSelfEvaluating (SEBoolean true))
TEST_UNIT "read_atom_bool_test2" = 
           assert_true (read_expression false1 = ExprSelfEvaluating 
           (SEBoolean false))
TEST_UNIT "read_atom_int_test1" = 
           assert_true (read_expression one = ExprSelfEvaluating (SEInteger 1))
TEST_UNIT "read_atom_int_test2" = 
           assert_true (read_expression neg_two = ExprSelfEvaluating 
           (SEInteger (-2)))
TEST_UNIT "read_atom_nil_test" = 
           assert_raises (Some (Failure "Unknown expression form")) 
           read_expression Nil

(*Test cases for reading Cons(<boolean>, b) and Cons(<integer>, b)*)
TEST_UNIT "read_cons_bool_test" = 
           assert_raises (Some (Failure "")) read_expression 
           (Cons (true1, (Cons (two, Nil))))
TEST_UNIT "read_cons_int_test" = 
           assert_raises (Some (Failure "")) read_expression 
           (Cons (one, (Cons (two, Nil))))
TEST_UNIT "read_cons_int_test" = 
           assert_raises (Some (Failure "")) read_expression 
           (Cons (one, (Cons (two, Nil))))

(*Test cases for reading variables*)
TEST_UNIT "read_expression_variable_test1" = 
                   assert_true (read_expression (Cons(Atom (Identifier 
                   (identifier_of_string "x")), Cons (Atom (Integer 1), 
                   Cons (Atom (Integer 2), Nil)))) = 
                   ExprProcCall (ExprVariable (variable_of_identifier 
                   (identifier_of_string "x")), ([ExprSelfEvaluating (SEInteger 1); 
                   ExprSelfEvaluating (SEInteger 2)])))
TEST_UNIT "read_expression_variable_test2" = 
                   assert_true (read_expression one_plus_two = 
                   ExprProcCall (ExprVariable (variable_of_identifier 
                   (identifier_of_string "+")), ([ExprSelfEvaluating (SEInteger 1); 
                   ExprSelfEvaluating (SEInteger 2)])))
TEST_UNIT "read_expression_variable_test3" = 
                   assert_true (read_expression one_minus_two = 
                   ExprProcCall (ExprVariable (variable_of_identifier 
                   (identifier_of_string "+")), ([ExprSelfEvaluating (SEInteger 1); 
                   ExprSelfEvaluating (SEInteger (-2))])))
TEST_UNIT "read_expression_variable_test4" = 
                   assert_true (read_expression one_plus_two_three = 
                   ExprProcCall (ExprVariable (variable_of_identifier 
                   (identifier_of_string "+")), ([ExprSelfEvaluating (SEInteger 1); 
                   ExprSelfEvaluating (SEInteger (2)); ExprSelfEvaluating (SEInteger (3))])))
TEST_UNIT "read_expression_variable_test5" = 
                   assert_true (read_expression x_plus_x = 
                   ExprProcCall (ExprVariable (variable_of_identifier 
                   (identifier_of_string "+")), ([ExprVariable (var_of_str "x"); 
                   ExprVariable (var_of_str "x")])))
TEST_UNIT "read_expression_variable_test6" = 
                   assert_true (read_expression x_plus_y = 
                   ExprProcCall (ExprVariable (variable_of_identifier 
                   (identifier_of_string "+")), ([ExprVariable (var_of_str "x"); 
                   ExprVariable (var_of_str "y")])))

(*Test cases for reading cons starting with keyword identifiers*)
(*Testing reading quote *)
TEST_UNIT "read_expression_quote_test1" = 
                   assert_true (read_expression quote1 = ExprQuote x)

TEST_UNIT "read_expression_quote_test2" = 
                   assert_true (read_expression quote2 = ExprQuote one) 
TEST_UNIT "read_expression_quote_test2" = 
                   assert_true (read_expression quote3 = ExprQuote one_plus_two)

(*Testing cases for reading if *)
TEST_UNIT "read_expression_if_test1" = 
          assert_true (read_expression if1 = ExprIf (read_expression true1, 
          read_expression one, read_expression two))
TEST_UNIT "read_expression_if_test2" = 
          assert_true (read_expression if2 = ExprIf (read_expression false1, 
          read_expression one, read_expression two))
TEST_UNIT "read_expression_if_test2" = 
          assert_true (read_expression if3 = ExprIf (read_expression one, 
          read_expression one, read_expression two))

(*Testing reading lambda *)
TEST_UNIT "read_expression_lambda_test1" = 
          assert_true (read_expression lambda1 = ExprLambda ([(var_of_str "x")],
          [(ExprVariable (var_of_str "x"))]))

(*Testing reading set! *)
TEST_UNIT "read_expression_set_test1" =
          assert_true (read_expression set1 = ExprAssignment ((var_of_str "x"),
          ExprSelfEvaluating (SEInteger 3)))

(*Testing reading let*)
TEST_UNIT "read_expression_let_test1" = 
          assert_true (read_expression let1 = ExprLet ([(var_of_str "x", 
          ExprSelfEvaluating (SEInteger 0));(var_of_str "y", 
          ExprSelfEvaluating (SEInteger 1))],[read_expression x_plus_y]))

(*Testing reading letstar *)
TEST_UNIT "read_expression_letstar_test1" = 
          assert_true (read_expression letstar1 = ExprLetStar 
          ([(var_of_str "x", ExprSelfEvaluating (SEInteger 0)); 
          (var_of_str "y", read_expression (Cons (plus_sign, 
          Cons (one, Cons (x, Nil)))))],[read_expression x_plus_y]))

(*Testing reading letrec *)
TEST_UNIT "read_expression_letrec_test1" = 
           assert_true (read_expression letrec1 = ExprLetRec ([(dat_to_var 
           factorial, read_expression (Cons (kw_lambda, Cons (Cons (x, Nil), 
           Cons (Cons (kw_if, Cons (Cons(equal, Cons (x, Cons (zero, Nil))), 
           Cons (one, Cons (Cons (mult_sign, Cons (x, Cons (Cons (factorial, 
           Cons (Cons (plus_sign, Cons (x, Cons (neg_one, Nil))), Nil)), 
           Nil))), Nil)))), Nil)))))], [read_expression (Cons (factorial, 
           Cons (five, Nil)))]))
TEST_UNIT "read_expression_letrec_test2" = 
           assert_true (read_expression letrec2 = ExprLetRec ([((var_of_str "even?"), 
           ExprLambda ([(var_of_str "x")], [(ExprIf ((ExprProcCall 
           ((ExprVariable (var_of_str "equal?")), [(ExprVariable (var_of_str "x"));
           (ExprSelfEvaluating (SEInteger 0))])), (ExprSelfEvaluating (SEBoolean true)),
           (ExprProcCall ((ExprVariable (var_of_str "odd?")), [(ExprProcCall 
           ((ExprVariable (var_of_str "+")), [(ExprVariable (var_of_str "x")); 
           (ExprSelfEvaluating (SEInteger (-1)))]))]))))]));
           ((var_of_str "odd?"), ExprLambda ([(var_of_str "x")], [(ExprIf 
           ((ExprProcCall ((ExprVariable (var_of_str "equal?")), [(ExprVariable 
           (var_of_str "x"));(ExprSelfEvaluating (SEInteger 0))])), 
           (ExprSelfEvaluating (SEBoolean false)), (ExprProcCall ((ExprVariable 
           (var_of_str "even?")), [(ExprProcCall ((ExprVariable (var_of_str "+")), 
           [(ExprVariable (var_of_str "x")); (ExprSelfEvaluating 
           (SEInteger (-1)))]))]))))]))], [(ExprProcCall ((ExprVariable 
           (var_of_str "even?")), [(ExprSelfEvaluating (SEInteger 5))]))]))

(*Testing reading define in read_expression*)
TEST_UNIT "read_expression_define_test1" = 
           assert_raises (Some (Failure 
           "define not allowed as an expression, only at the toplevel")) 
           read_expression def1

(*Testing reading invalid datum*)
let invalid = Cons (kw_quote, Cons (x, one))
TEST_UNIT "read_expression_invalid_test1" = 
           assert_raises (Some (Failure "Invalid datum")) read_expression invalid

(***************************TEST CASES FOR READ_TOPLEVEL******************************)


(*Test cases for reading define at toplevel*)
TEST_UNIT "read_toplevel_define_test1" = assert_true 
          (read_toplevel define1 = ToplevelDefinition ((var_of_str "id"), 
           ExprLambda ([(var_of_str "x")], [(ExprVariable (var_of_str "x"))])))

TEST_UNIT "read_toplevel_define_test2" = assert_true 
           (read_toplevel define2 = ToplevelDefinition ((var_of_str "f"), 
           ExprLambda ([(var_of_str "x");(var_of_str "y")], 
           [(read_expression x_plus_y)])))

TEST_UNIT "read_toplevel_define_test3" = assert_true 
           (read_toplevel define3 = ToplevelDefinition ((var_of_str "-"), 
           ExprLambda ([(var_of_str "x");(var_of_str "y")], [(ExprProcCall 
           ((ExprVariable (var_of_str "+")), [(ExprVariable (var_of_str "x"));
           (ExprProcCall ((ExprVariable (var_of_str "*")), 
           [(ExprVariable (var_of_str "y"));(ExprSelfEvaluating 
           (SEInteger (-1)))]))]))])))

TEST_UNIT "read_toplevel_define_test4" = assert_true 
           (read_toplevel define4 = ToplevelDefinition ((var_of_str "a"), 
           (ExprSelfEvaluating (SEInteger 1))))

TEST_UNIT "read_toplevel_define_test5" = assert_true 
           (read_toplevel define5 = ToplevelDefinition ((var_of_str "f"), 
           ExprLambda ([(var_of_str "x")], [(ExprProcCall 
           ((ExprVariable (var_of_str "+")), [(ExprVariable (var_of_str "a"));
           (ExprVariable (var_of_str "x"))]))])))

(*Testing reading_toplevel for TopLevelExpression*)
TEST_UNIT "read_toplevel_expression_test1" = assert_true 
           (read_toplevel let1 = ToplevelExpression(ExprLet ([(var_of_str "x", 
           ExprSelfEvaluating (SEInteger 0));(var_of_str "y", 
           ExprSelfEvaluating (SEInteger 1))],[read_expression x_plus_y])))


(***************************TEST CASES FOR EVAL******************************)


let init = initial_environment()
(*Add {x=2} to initial environment*)
let env = add_binding init (var_of_str "x", ref (ValDatum (Atom (Integer 2))))

(*Test cases for evaluating ExprSelfEvaluating*)
TEST_UNIT "eval_ExprSelfEvaluating_test1" = 
            assert_true (eval (ExprSelfEvaluating (SEBoolean true)) env = 
            ValDatum (Atom (Boolean true)))
TEST_UNIT "eval_ExprSelfEvaluating_test2" = 
            assert_true (eval (ExprSelfEvaluating (SEInteger 1)) env = 
            ValDatum (Atom (Integer 1)))
TEST_UNIT "eval_ExprSelfEvaluating_test2" = 
            assert_true (eval (ExprSelfEvaluating (SEInteger (-1))) env = 
            ValDatum (Atom (Integer (-1))))

(*Test cases for evaluating ExprVariable*)
TEST_UNIT "eval_ExprVariable_test1" = 
            assert_true (eval (ExprVariable (variable_of_identifier 
            (identifier_of_string "x"))) env = ValDatum (Atom (Integer 2)))
TEST_UNIT "eval_ExprVariable_test2" = 
            assert_true (eval (ExprVariable (variable_of_identifier 
            (identifier_of_string "course"))) env = ValDatum (Atom (Integer 3110)))
(*Testing evaluating after set! existing variable in env*)
let set_x_to_3 = read_expression (Cons(kw_set, Cons(x, Cons(three, Nil))))
TEST_UNIT "eval_ExprVariable_test3" = 
            assert_true (eval set_x_to_3 env = ValDatum Nil)
TEST_UNIT "eval_ExprVariable_test4" = 
            assert_true (eval (ExprVariable (variable_of_identifier 
            (identifier_of_string "x"))) env = ValDatum (Atom (Integer 3)))
(*Testing evaluating after define*)
let env2 = snd(eval_toplevel (read_toplevel define7) env)
TEST_UNIT "eval_ExprVariable_test5" = 
            assert_true (eval (ExprVariable (variable_of_identifier 
            (identifier_of_string "x"))) env2 = ValDatum (Atom (Integer 3110)))  
(*Testing evaluating variable that doesn't exist in environment*)
TEST_UNIT "eval_ExprVariable_test6" = 
            assert_raises (Some (Failure "Variable is not bound in the environment")) 
            (eval (ExprVariable (variable_of_identifier 
            (identifier_of_string "DSFSDF")))) env2

(*Test cases for evaluating ExprQuote*)
TEST_UNIT "eval_ExprQuote_test1" = assert_true (eval (ExprQuote x) env = ValDatum x)
TEST_UNIT "eval_ExprQuote_test2" = assert_true (eval (ExprQuote one_plus_two) env = 
                                   ValDatum one_plus_two)

(*Test cases for evaluating ExprLambda
  Test case included in test cases for evaluating ExprLetRec*)

(*Test cases for evaluating ExprProcCall*)
TEST_UNIT "eval_ExprProcCall_test1" = 
            assert_true (eval (ExprProcCall (ExprVariable (variable_of_identifier 
            (identifier_of_string "+")), ([ExprSelfEvaluating (SEInteger 1); 
            ExprSelfEvaluating (SEInteger 2)]))) env = ValDatum (Atom (Integer 3)))
TEST_UNIT "eval_ExprProcCall_test2" = 
            assert_true (eval (ExprProcCall (ExprVariable (variable_of_identifier 
            (identifier_of_string "+")), ([ExprSelfEvaluating (SEInteger 1); 
            ExprSelfEvaluating (SEInteger (-2))]))) env = ValDatum (Atom (Integer (-1))))
TEST_UNIT "eval_ExprProcCall_test2" = 
            assert_true (eval (ExprProcCall (ExprVariable (variable_of_identifier 
            (identifier_of_string "+")), ([ExprSelfEvaluating (SEInteger 1); 
            ExprSelfEvaluating (SEInteger (2)); ExprSelfEvaluating 
            (SEInteger (3))]))) env = ValDatum (Atom (Integer 6)))

(*Test cases for evaluating ExprIf*)
(*Testing when first expression is true*)
TEST_UNIT "eval_ExprIf_test1" = 
           assert_true (eval (ExprIf (read_expression true1, 
           read_expression one, read_expression two)) env = 
           ValDatum (Atom (Integer 1)))
(*Testing when first expression is false*)
TEST_UNIT "eval_ExprIf_test2" = 
           assert_true (eval (ExprIf (read_expression false1, 
           read_expression one, read_expression two)) env = 
           ValDatum (Atom (Integer 2)))
(*Testing when first expression is not true, but also not necessarily false*)
TEST_UNIT "eval_ExprIf_test2" = 
           assert_true (eval (ExprIf (read_expression one, 
           read_expression one, read_expression two)) env = 
           ValDatum (Atom (Integer 1)))           

(*Test cases for evaluating ExprAssignment*)
(*Testing assigning value to an existing variable in env*)
TEST_UNIT "eval_Expr_Assignment_test1" = 
           assert_true (eval (ExprAssignment ((var_of_str "x"), ExprSelfEvaluating 
           (SEInteger 123))) env = ValDatum Nil)
TEST_UNIT "eval_Expr_Assignment_test2" = 
           assert_true (is_bound env (var_of_str "x"))
TEST_UNIT "eval_Expr_Assignment_test3" = 
           assert_true (!(get_binding env (var_of_str "x")) = 
           ValDatum (Atom (Integer 123)))
(*Testing assigning value to a non-existing variable in env*)
TEST_UNIT "eval_Expr_Assignment_test4" = 
           assert_raises (Some (Failure "identifier is not bound in this environment"))
           (eval (ExprAssignment ((var_of_str "non"), ExprSelfEvaluating 
           (SEInteger 123)))) env

(*Test cases for evaluating ExprLet*)
TEST_UNIT "eval_ExprLet_test1" = assert_true (eval (ExprLet ([(var_of_str "x", 
           ExprSelfEvaluating (SEInteger 0));(var_of_str "y", 
           ExprSelfEvaluating (SEInteger 1))],[read_expression x_plus_y])) env = 
           ValDatum (Atom (Integer 1)))

(*Test cases for evaluating ExprLetStar*)
TEST_UNIT "eval_ExprLetStar_test1" = assert_true (eval (ExprLetStar 
           ([(var_of_str "x", ExprSelfEvaluating (SEInteger 0)); 
           (var_of_str "y", read_expression (Cons (plus_sign, 
           Cons (one, Cons (x, Nil)))))],[read_expression x_plus_y])) env = 
           ValDatum (Atom (Integer 1)))

(*Test cases for evaluating ExprLetRec*)
TEST_UNIT "eval_ExprLetRec_test1" = assert_true (eval (ExprLetRec ([(dat_to_var 
           factorial, read_expression (Cons (kw_lambda, Cons (Cons (x, Nil), 
           Cons (Cons (kw_if, Cons (Cons(equal, Cons (x, Cons (zero, Nil))), 
           Cons (one, Cons (Cons (mult_sign, Cons (x, Cons (Cons (factorial, 
           Cons (Cons (plus_sign, Cons (x, Cons (neg_one, Nil))), Nil)), 
           Nil))), Nil)))), Nil)))))], [read_expression (Cons (factorial, 
           Cons (five, Nil)))])) env  = ValDatum (Atom (Integer 120)))
TEST_UNIT  "eval_ExprLetRec_test2" = assert_true (eval (ExprLetRec ([((var_of_str "even?"), 
           ExprLambda ([(var_of_str "x")], [(ExprIf ((ExprProcCall 
           ((ExprVariable (var_of_str "equal?")), [(ExprVariable (var_of_str "x"));
           (ExprSelfEvaluating (SEInteger 0))])), (ExprSelfEvaluating (SEBoolean true)),
           (ExprProcCall ((ExprVariable (var_of_str "odd?")), [(ExprProcCall 
           ((ExprVariable (var_of_str "+")), [(ExprVariable (var_of_str "x")); 
           (ExprSelfEvaluating (SEInteger (-1)))]))]))))]));
           ((var_of_str "odd?"), ExprLambda ([(var_of_str "x")], [(ExprIf 
           ((ExprProcCall ((ExprVariable (var_of_str "equal?")), [(ExprVariable 
           (var_of_str "x"));(ExprSelfEvaluating (SEInteger 0))])), 
           (ExprSelfEvaluating (SEBoolean false)), (ExprProcCall ((ExprVariable 
           (var_of_str "even?")), [(ExprProcCall ((ExprVariable (var_of_str "+")), 
           [(ExprVariable (var_of_str "x")); (ExprSelfEvaluating (SEInteger (-1)))]))]))))]))],
           [(ExprProcCall ((ExprVariable (var_of_str "even?")), 
           [(ExprSelfEvaluating (SEInteger 5))]))])) env  = ValDatum (Atom (Boolean false)))


(******************************TEST CASES FOR EVAL_TOPLEVEL**********************************)

(*Testing evaluating toplevel definition*)
let init1 = initial_environment ()
let def3 = Cons (kw_define, (Cons (x, Cons (five, Nil))))
let new_env = snd(eval_toplevel (read_toplevel (def3)) init1)
(*Check that the define function created binding {x = 5}*)
TEST_UNIT "eval_toplevel_test1" = 
           assert_true (!(get_binding new_env (var_of_str "x")) = 
           ValDatum (Atom (Integer (5))))
(*Testing evaluating toplevel expression*)
TEST_UNIT "eval_toplevel_test2" = 
           assert_true (fst(eval_toplevel (read_toplevel let1) init1) = 
           ValDatum one)
 
(******************************TEST CASES FOR INITIAL_ENVIRONMENT****************************)

let init2 = initial_environment()

let cons = Atom (Identifier (identifier_of_string "cons"))
let car = Atom (Identifier (identifier_of_string "car"))
let cdr = Atom (Identifier (identifier_of_string "cdr"))
let var_eval = Atom (Identifier (identifier_of_string "eval"))

let proccall_cons = Cons(cons, Cons(one, Cons(two, Nil)))
let proccall_car = Cons(car, Cons(proccall_cons, Nil))
let proccall_cdr = Cons(cdr, Cons(proccall_cons, Nil))
let proccall_plus = Cons(plus_sign, Cons(one, Cons(two, Nil)))
let proccall_mult = Cons(mult_sign, Cons(one, Cons(zero, Nil)))
let proccall_equal = Cons(equal, Cons(one, Cons(one, Nil)))
let proccall_equal2 = Cons(equal, Cons(one, Cons(two, Nil)))
let proccall_eval = Cons(var_eval, Cons(proccall_plus, Nil))

TEST_UNIT "initial_environment_test1" = assert_true 
          (eval (read_expression (proccall_cons)) init2 = ValDatum (Cons(one, two)))
TEST_UNIT "initial_environment_test2" = assert_true
          (eval (read_expression (proccall_car)) init2 = ValDatum (one))
TEST_UNIT "initial_environment_test3" = assert_true
          (eval (read_expression (proccall_cdr)) init2 = ValDatum (two))
TEST_UNIT "initial_environment_test4" = assert_true
          (eval (read_expression (proccall_plus)) init2 = ValDatum (three))
TEST_UNIT "initial_environment_test5" = assert_true
          (eval (read_expression (proccall_mult)) init2 = ValDatum (zero))
TEST_UNIT "initial_environment_test6" = assert_true
          (eval (read_expression (proccall_equal)) init2 = ValDatum (true1))
TEST_UNIT "initial_environment_test7" = assert_true
          (eval (read_expression (proccall_eval)) init2 = ValDatum (three))
