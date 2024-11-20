exception NotImplemented

exception Msg of string
let invalidInput = Msg("invalidInput (use variable indices as integers)")

type expression =
  | Var of int
  | Not of expression
  | And of expression * expression
  | Or of expression * expression

(* HELPER FUNCTION *)
(** Matches a variable's value in the provided list of (int*bool) pairs *)
let matchValue u values = 
  raise NotImplemented

(* PRINTING FUNCTIONS *)
(** Converts a boolean expression into a human-readable string *)
let rec printExpression (e: expression) : string = 
  (* Recursive version here *)
  ""

let printExpression' (e: expression) : string =
  (* Tail-recursive version with continuations *)
  ""
(* INPUT COLLECTION *)
(** Collects list of unique variable indices used in a boolean expression *)
let inputList (e: expression): int list = 
  (* Recursive version here *)
  []

let trInputList (e: expression): int list =
  (* Tail-recursive version with continuations *)
  []

(* EVALUATION FUNCTIONS *)

(** Evaluates a boolean expression using memoization *)
let memoEvaluateExpression =
  (* Memoization store initialization *)
  let store = Hashtbl.create 1000 in
  let rec eval (e: expression) (values: (int * bool) list) = 
    (* Memoization logic and evaluation *)
    false
  in
  eval

(** Evaluates a boolean expression using a standard recursive approach *)
let evaluateExpression =
  let rec eval (e: expression) (values: (int * bool) list) = 
    (* Recursive evaluation logic *)
    false
  in
  eval

(** Evaluates a boolean expression using a tail-recursive approach with continuations *)
let evaluateExpression' =
  let rec trEval (acc: bool -> bool) (e: expression) (values: (int * bool) list) = 
    (* Tail-recursive evaluation logic *)
    acc true
  in
  trEval (fun r -> r)

(* TRUTH TABLES *)

(** Generates all possible combinations of boolean values for a given number of variables *)
let rec generateCombinations n : (bool list) list = 
  (* Combination generation logic *)
  []

(** Constructs a truth table for the given expression *)
let truthTable evaluator (e: expression) : (bool list * bool) list =
  (* Constructing truth table *)
  []

(** Prints the truth table for a given expression *)
let printTruthTable evaluator (e: expression) = 
  (* Printing logic *)
  ()

(* SOLUTION SET FUNCTIONS *)

(** Creates a boolean list of length n filled with true *)
let makeTrueList n = 
  (* Creating true list *)
  []

(** Determines if a boolean expression is always true for all variable combinations *)
let alwaysTrue evaluator (e: expression) : bool = 
  (* Check if always true *)
  false

(** Determines if there exists at least one solution for which the boolean expression is true *)
let existsSolution evaluator (e: expression) : bool = 
  (* Check if there exists a solution *)
  false

(** Finds all solutions for which a boolean expression evaluates to true *)
let findSolutions evaluator (e: expression) = 
  (* Finding solutions *)
  []

(* SAT FUNCTIONS *)

(** Checks if one boolean expression implies another *)
let satSolverImplies evaluator (e1: expression) (e2: expression) : bool = 
  (* Implication check logic *)
  false

(** Checks if one boolean expression is implied by another *)
let satSolverImpliedBy evaluator (e1: expression) (e2: expression) : bool = 
  (* Implied by logic *)
  false

(** Checks if two boolean expressions are logically equivalent *)
let satSolverIff evaluator (e1: expression) (e2: expression) : bool = 
  (* Logical equivalence check *)
  false








