(**
Boolean Expression Evaluator and SAT Solver
This program includes capabilities such as 
  - Define and manipulate boolean expressions
  - Generate truth tables
  - Evaluate generated boolean expressions 
  - Utilize memoization to optimize processes
  - Solve SAT (satifiability) problems 
  *)

(** Exception raised for invalid input such as non-integers *)
exception Msg of string
let invalidInput = Msg("invalidInput (use variable indices as integers)")
let testFailed = Msg("Test has Failed!")
(** Represents elements of a boolean expression *)
type expression =
  | Var of int
  | Not of expression
  | And of expression * expression
  | Or of expression * expression

(* HELPER FUNCTIONS *)
(** Matches a variable's value in the provided list of (int*bool) pairs
  @param u The variable to look up
  @param values A list of (variable, value) pairs
  @return The value of the variable if found
  @raise Msg If variable not found in the list
*)
let matchValue u values = 
  try List.assoc u values with Not_found -> raise invalidInput
(**
  Converts an integer to a string and adds whitespace to make it 6 characters wide.
    @param n Integer to format.
    @return The resulting string.
*) 
let formatted_int n =
  let s = string_of_int n in
  let space = String.make (6 - String.length s) ' ' in
  s ^ space
(**
  Converts an boolean to a string and adds whitespace to make it 6 characters wide.
    @param n A boolean value.
    @return The resulting string.
*) 
let formatted_bool b =
  if b then "true " else "false"


(* PRINTING *)
(**
  Converts boolean expression into a human-readable string (recursive and tail-recursive)
  @param e The expression to convert
  @return A string representation of the expression
*)
let rec printExpression (e: expression): string = match e with (* recursive *)
  | Var(u) -> string_of_int u
  | Not(u) -> "¬" ^ printExpression u
  | And(u, v) -> "(" ^ printExpression u ^ " ∧ " ^ printExpression v ^ ")"
  | Or(u, v) -> "(" ^ printExpression u ^ " ∨ " ^ printExpression v ^ ")"
let printExpression' (e: expression) : string = (* with continuations *)
  let rec trPrintExpression (e: expression) (acc: string -> string): string = match e with
    |Var(u) -> acc (string_of_int u)
    |Not(u) -> trPrintExpression u (fun r -> acc ("¬" ^ r))
    |And(u, v) -> trPrintExpression u (fun r1  -> trPrintExpression v (fun r2 -> acc ("(" ^ r1 ^ " ∧ " ^ r2 ^ ")")))
    |Or(u, v) -> trPrintExpression u (fun r1  -> trPrintExpression v (fun r2 -> acc ("(" ^ r1 ^ " ∨ " ^ r2 ^ ")"))) in
  trPrintExpression e (fun r -> r)


(* INPUT COLLECTION *)
(**
  Collects list of unique variable indices used in a boolean expression 
  (recursive and tail-recursive versions), in increasing order.
  @param e The expression to analyze
  @return A sorted list of unique variable indices
*)
let inputList (e: expression): int list = (* recursive *)
  let rec matchInputs e acc = match e with
    | Var u -> if List.mem u acc then acc else u :: acc
    | Not u -> matchInputs u acc
    | And(u, v) | Or(u, v) -> matchInputs u (matchInputs v acc)
  in
  List.sort (fun a b -> if a<b then 0 else 1) (matchInputs e [])
let trInputList (e: expression): int list = (* with continuations *)
  let rec matchInputs e acc sc = match e with
    | Var u -> if List.mem u acc then sc acc else sc (u :: acc)
    | Not u -> matchInputs u acc sc
    | And(u, v) | Or(u, v) -> matchInputs u acc (fun r -> sc (matchInputs v r sc))
  in
  List.sort (fun a b -> if a<b then 0 else 1) (matchInputs e [] (fun r -> r))


(* EVALUATION FUNCTIONS *)
(** EVALUATING REGULARLY 
Evaluates a boolean expression using a standard recursive approach.
    @param e The boolean expression to evaluate.
    @param values A list of `(variable, value)` pairs representing the variable assignments.
    @return The result of evaluating the expression with the given variable assignments.
*)
let evaluateExpression =
  let rec eval (e: expression) (values: (int * bool) list) = match e with 
    |Var(u) -> matchValue u values
    |Not(u) -> not (eval u values)
    |And(u, v) -> (eval u values) && (eval v values)
    |Or(u, v) -> (eval u values) || (eval v values) in
  eval

(** EVALUATING TAIL RECURSIVELY 
Evaluates a boolean expression using a tail-recursive approach with continuations.
    @param e The boolean expression to evaluate.
    @param values A list of `(variable, value)` pairs representing the variable assignments.
    @return The result of evaluating the expression with the given variable assignments.
  *)
let evaluateExpression' =
    let rec trEval (acc: bool -> bool) (e: expression) (values: (int * bool) list) = match e with 
      |Var(u) -> acc (matchValue u values)
      |Not(u) -> trEval (fun r -> acc (not r)) u values
      |And(u, v) -> trEval (fun r1 -> trEval (fun r2 -> acc (r1 && r2)) v values) u values
      |Or(u, v) -> trEval (fun r1 -> trEval (fun r2 -> acc (r1 || r2)) v values) u values in
    trEval (fun r -> r)

(** EVALUATING WITH MEMOIZATION 
  Evaluates a boolean expression with memoization to optimize repeated evaluations
    @param e The expression to evaluate.
    @param values A list of (variable, value) pairs.
    @return The result of evaluating the expression.
*)
let memoEvaluateExpression =
  let store : (expression * (int * bool) list, bool) Hashtbl.t = Hashtbl.create 1000 in

  let rec eval (e: expression) (values: (int * bool) list) =
    (* Check memoization store *)
    match Hashtbl.find_opt store (e, values) with
    | Some result -> result
    | None ->
        (* Evaluate based on expression type *)
        let result = match e with
          | Var u -> matchValue u values
          | Not u -> not (eval u values)
          | And(u, v) -> (eval u values) && (eval v values)
          | Or(u, v) -> (eval u values) || (eval v values)
        in
        (* Memoize and return result *)
        (Hashtbl.add store (e, values) result;
        result)
      in eval

(* TRUTH TABLES *)

(** Generates all possible combinations of boolean values for a given number of variables.
    @param n The number of variables.
    @return A list of boolean combinations.
*)
let rec generateCombinations n : (bool list) list = (* will generate in binary decreasing order *)
  if n = 0 then [[]]
  else
    let sub_combinations = generateCombinations (n - 1) in
      List.map (fun comb -> true :: comb) sub_combinations @
      List.map (fun comb -> false :: comb) sub_combinations

(**
   Constructs a truth table for the given expression.
    
    @param evaluator The evaluation function to use.
    @param e The boolean expression.
    @return A list of tuples, where each tuple consists of:
            - A boolean combination for the variables (bool list).
            - The evaluation result for the expression (bool).
*)
let truthTable evaluator (e : expression) : (bool list * bool) list =
  let vars = inputList e in
  let combinations = generateCombinations (List.length vars) in
  List.map (fun comb ->
      let values = List.combine vars comb in
      (comb, evaluator e values)
    ) combinations

    
(**
  Prints the truth table for a given expression.
    @param evaluator The evaluation function to use.
    @param e The boolean expression.
*)
let printTruthTable evaluator (e : expression) =
  let vars = inputList e in
  let table = truthTable evaluator e in
  Printf.printf "Truth table for %s:\n" (printExpression e);
  Printf.printf "%12s| Result\n" (String.concat "" (List.map formatted_int vars));
  List.iter (fun (comb, result) ->
      Printf.printf "%11s | %B\n" (String.concat " " (List.map formatted_bool comb)) result
    ) table

(*SOLUTION SET FUNCTIONS*)
(**
  Creates a bool list of length n of true.
    @param n Length of list.
*)
let makeTrueList n = 
  let rec makeTrueList' n acc =
    if n = 0 then acc else makeTrueList' (n-1) (true::acc) in
  makeTrueList' n []

(** 
    Determines if a boolean expression is always true for all variable combinations.
    
    @param evaluator The evaluation function to use.
    @param e The boolean expression.
    @return `true` if the expression is always true, otherwise `false`.
*)
let alwaysTrue evaluator (e: expression) : bool = 
  let table = truthTable evaluator e in
  let resultants = List.map (fun (a, b) -> b) table in
  resultants = makeTrueList (List.length table)

(** 
  Determines if there exists at least one solution for which the boolean expression is true.
  
  @param evaluator The evaluation function to use.
  @param e The boolean expression.
  @return `true` if a solution exists, otherwise `false`.
*)
let existsSolution evaluator (e: expression) : bool = 
  let table = truthTable evaluator e in
  let resultants = List.map (fun (a, b) -> b) table in
  List.mem true resultants

(**
    Finds all solutions (variable assignments) for which a boolean expression evaluates to `true`.
    
    @param evaluator The evaluation function to use.
    @param e The boolean expression.
    @return A list of variable assignments (as lists of `(variable, value)` pairs) that satisfy the expression.
*)
let findSolutions evaluator (e: expression) = 
  let table = truthTable evaluator e in
  List.filter (fun (a, b) -> b) table




(* SAT FUNCTIONS *)
(** 
  Checks if one boolean expression implies another.
  
  @param evaluator The evaluation function to use.
  @param e1 The first boolean expression.
  @param e2 The second boolean expression.
  @return `true` if `e1` implies `e2`, otherwise `false`.
*)
let satSolverImplies evaluator (e1 : expression) (e2 : expression) : bool = 
  let e = Or(Not(e1), e2) in
  alwaysTrue evaluator e

(** 
    Checks if one boolean expression is implied by another.
    
    @param evaluator The evaluation function to use.
    @param e1 The first boolean expression.
    @param e2 The second boolean expression.
    @return `true` if `e2` implies `e1`, otherwise `false`.
*)
let satSolverImpliedBy evaluator (e1 : expression) (e2: expression) : bool = 
  let e = Or(Not(e2), e1) in
  alwaysTrue evaluator e

(** 
    Checks if two boolean expressions are logically equivalent.
    
    @param evaluator The evaluation function to use.
    @param e1 The first boolean expression.
    @param e2 The second boolean expression.
    @return `true` if `e1` is equivalent to `e2`, otherwise `false`.
*)
let satSolverIff (e1 : expression) (e2: expression) evaluator : bool = 
  let e = Or(And(e1, e2), And(Not(e1), Not(e2))) in
  alwaysTrue evaluator e




(* TESTING *) 
let test_and = And((Var(1)), Var(2))
let test_or = Or((Var(1)), Var(2))
let test_not = Not(Var(1))
let test1 = And(Not(Or(Var(1), And(Var(1), Var(2)))), Or(And(Var(3), Var(2)), And(Not(Var(1)), Var(3))))
let test2 = And(Not(Var(3)), Var(2))
let test3 = And(And(Or(Or(Not(Var(3)), Var(10)), Or(Not(Var(3)), And(Var(2), Var(9)))), 
                    Or(Var(1), Or(Or(Var(6), Var(5)), Var(2)))), Or(Or(Var(5), Or(Or(Var(8), Var(3)), Not(Var(5)))), Not(Var(10))))
let test4 = Or(And((Var(1)),Not(Var(2))), And(Not(Var(1)),(Var(2)))) 
let test5 = Not(Or(And(Var(1),Var(2)),Not(Var(3))))
let test6 = Or(Not(And(Var(1),Var(2))),And(Var(3),Var(4)))
let test7 = And(Not(Or(Var(1),And(Var(2),Var(3)))),Or(And(Not(Var(4)),Var(3)),And(Var(1),Or(Var(2),Var(4)))))

(** 
    Checks to see if the output of running the evaluation function matches 
what is expected.
    
    @param evaluator The evaluation function to use.
    @param e The boolean expression.
    @param e A boolean list that contains the correct evaluation.
*)    
let run_test evaluator (e : expression) expected =
  let table = truthTable evaluator e in
  let result = List.map snd table in
  if result = expected then
    Printf.printf "Pass: %s\n" (printExpression e)
  else
    Printf.printf "Fail: %s\nExpected: %s\nReceived: %s\n\n"
      (printExpression e)
      (String.concat "; " (List.map string_of_bool expected))
      (String.concat "; " (List.map string_of_bool result))    ;;

      
(* Executes tests for a specific evaluator function *)
let run_tests eval () =
  run_test eval test_and [true; false; false; false];
  run_test eval test_or [true; true; true; false];
  run_test eval test_not [false; true;];
  run_test eval test1 [false; false; false; false; true; false; true; false];
  run_test eval test2 [false; true; false; false];
  run_test eval test4 [false; true; true; false];
  run_test eval test5 [false; false; true; false; true; false; true; false];
  
  run_test eval test6 [true; false; false; false; true; true; true; 
                       true; true; true; true; true; true; true; true; true];
  
  run_test eval test7 [false; false; false; false; false; false; false;
                       false; false; false; false; false; false; true; false; false];;

(* This function executes all tests for each evaluator function. *)
let run_all_tests () = 
  run_tests evaluateExpression();
  run_tests evaluateExpression'();
  run_tests memoEvaluateExpression();;
run_all_tests();;
