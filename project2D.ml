exception Msg of string
let invalidInput = Msg("invalidInput (use \"a\" or \"b\")")

type expression =
  | Var of string
  | Not of expression
  | And of expression * expression
  | Or of expression * expression;;

let combinations = [[true;true];[true;false];[false;true];[false;false]]

(*PRINTING*)
let rec printExpression (e: expression): string = match e with
  |Var("a") -> "a"
  |Var("b") -> "b"
  |Not(u) -> "¬" ^ printExpression u
  |And(u, v) -> "(" ^ printExpression u ^ " ∧ " ^ printExpression v ^ ")"
  |Or(u, v) -> "(" ^ printExpression u ^ " ∨ " ^ printExpression v ^ ")"
  |Var(_) -> raise invalidInput

let printExpression' (e: expression) : string =
  let rec trPrintExpression (e: expression) (acc: string -> string): string = match e with
    |Var("a") -> acc "a"
    |Var("b") -> acc "b"
    |Not(u) -> trPrintExpression u (fun r -> acc ("¬" ^ r))
    |And(u, v) -> trPrintExpression u (fun r1  -> trPrintExpression v (fun r2 -> acc ("(" ^ r1 ^ " ∧ " ^ r2 ^ ")")))
    |Or(u, v) -> trPrintExpression u (fun r1  -> trPrintExpression v (fun r2 -> acc ("(" ^ r1 ^ " ∨ " ^ r2 ^ ")")))
    |Var(_) -> raise invalidInput in

  trPrintExpression e (fun r -> r)


(*EVALUATING*)
let evaluateExpression =
  let pullValues e values = match values with 
  | a::[b] -> (

  let rec eval e = match e with 
    |Var("a") -> a
    |Var("b") -> b
    |Not(u) -> not (eval u)
    |And(u, v) -> let u' = (eval u) and v' = (eval v) in (u' && v')
    |Or(u, v) -> let u' = (eval u) and v' = (eval v) in (u' || v')
    |Var(_) -> raise invalidInput in
    eval e)

  |_-> raise invalidInput in
  pullValues
let evaluateExpression' =
  let pullValues e values = match values with 
  | a::[b] -> (

    let rec trEval e (acc: bool -> bool) = match e with 
      |Var("a") -> acc a
      |Var("b") -> acc b
      |Not(u) -> trEval u (fun r -> acc (not r))
      |And(u, v) -> trEval u (fun r1 -> trEval v (fun r2 -> acc (r1 && r2))) 
      |Or(u, v) -> trEval  u (fun r1 -> trEval v (fun r2 -> acc (r1 || r2)))
      |Var(_) -> raise invalidInput in
      trEval e (fun r -> r))

  |_ -> raise invalidInput in
  pullValues
let memoEvaluateExpression =
  let pullValues e values = match values with
  |a::[b] -> (

  let store = Hashtbl.create 1000 in
  let rec memoEval e =
    match Hashtbl.find_opt store e with
    | Some result -> result
    | None ->
        let result = match e with
          |Var("a") -> a
          |Var("b") -> b
          |Not(u) -> not (memoEval u)
          |And(u, v) -> (memoEval u) && (memoEval v)
          |Or(u, v) -> (memoEval u) || (memoEval v)
          |_ -> raise invalidInput
        in
        (Hashtbl.add store e result; result) in
        memoEval e)

    |_ -> raise invalidInput in 
    pullValues


(* TRUTH TABLE GENERATION *)
let truthTable2D evaluator (e : expression)  = 
    List.map (fun comb -> (comb, evaluator e comb)) combinations
let printTruthTable2D evaluator (e : expression) =
  let table = truthTable2D evaluator e in
  Printf.printf "Truth table for %s:\n" (printExpression e);
  Printf.printf "%12s | Result\n" "a   b";
  List.iter (fun (comb, result) ->
      Printf.printf "%12s | %B\n" (String.concat " " (List.map string_of_bool comb)) result
    ) table 

(*SAT SECTION*)
let alwaysTrue evaluator (e: expression) : bool = 
  let table = truthTable2D evaluator e in
  let resultants = List.map (fun (a, b) -> b) table in
  resultants = [true;true;true;true]
let existsSolution evaluator (e: expression) : bool = 
  let table = truthTable2D evaluator e in
  let resultants = List.map (fun (a, b) -> b) table in
  List.mem true resultants

let satSolverImplies evaluator (e1 : expression) (e2 : expression) : bool = 
  let e = Or(Not(e1), e2) in
  alwaysTrue evaluator e
let satSolverImpliedBy evaluator (e1 : expression) (e2: expression) : bool = 
  let e = Or(Not(e2), e1) in
  alwaysTrue evaluator e
let satSolverIff (e1 : expression) (e2: expression) evaluator : bool = 
  let e = Or(And(e1, e2), And(Not(e1), Not(e2))) in
  alwaysTrue evaluator e

  
(*SOLUTION SET*)
let findSolutions (e: expression) evaluator =
  let rec findComb comb acc = match comb with
  |[] -> acc
  |h::t -> if evaluator e h then findComb t (h::acc) else findComb t acc 
in findComb combinations []




(*    TESTINGGGGGG   *)
let test1 = And(Not(Or(Var("a"), And(Var("a"), Var("b")))), Or(And(Var("a"), Var("b")), And(Not(Var("a")), Var("b"))));;
let test2 = And(Not(Var("a")), Var("b")) ;;

let sat1 = And(Var("a"), Var("b"));;
let sat2 = Var("a")
let sat3 = Or(Var("a"), Var("b"))
let sat4 = Not(And(Not(Var("a")), Not(Var("b"))));;
printTruthTable2D evaluateExpression test2;;
printTruthTable2D evaluateExpression' test2;;
printTruthTable2D memoEvaluateExpression test2;;
alwaysTrue evaluateExpression (Not((And(Not(Var("a")), Var("a")))));;

printTruthTable2D evaluateExpression sat4;;
printTruthTable2D evaluateExpression' sat4;;



(*
let test e = 
  let a = evaluateExpression e (true, true) in 
  let a' = evaluateExpression' e (true, true) in 
  let a'' = memoEvaluateExpression e (true, true) in
  let c = printExpression e in
  let d = printExpression' e in
  let f = truthTable2D e evaluateExpression in
  let f' = truthTable2D e evaluateExpression' in
  let f'' = truthTable2D e memoEvaluateExpression in
  let g = findSolutions e evaluateExpression in

  "Reg: " ^ string_of_bool a ^ " TR: " ^ string_of_bool a' ^ " MEMO:  " ^ string_of_bool a'' ^
  "   Print Reg: " ^ c ^ " TR: " ^ d ^ "    " ^ "REG:  " ^string_of_bool_list_list f ^
  " TR : " ^ string_of_bool_list_list f' ^ " MEMO : " ^ string_of_bool_list_list f'' ^
  "FIND SOL: " ^ string_of_2_bool_list g;;

test test1;;
test test2;;
test sat1;;
test sat2;;
test sat3;;
truthTable2D test2 evaluateExpression;;
satSolverImplies sat3 sat4 evaluateExpression;;
satSolverImpliedBy sat3 sat4 evaluateExpression;;
satSolverIff sat3 sat4 evaluateExpression;;

memoEvaluateExpression test1 (false, true);;
existsSolution test1 evaluateExpression;;
truthTable2D test1 evaluateExpression;;
findSolutions test1 evaluateExpression

(*evaluateExpression test2 (true, true);;
evaluateExpression' test2 (true, true);;
printExpression test2;;
printExpression' test2;;
truthTable2D test2;;*)
let string_of_bool_list lst =
  "[" ^ String.concat "; " (List.map string_of_bool lst) ^ "]"
let string_of_2_bool_list lst =
  "[" ^ String.concat "; " (List.map (fun (a, b) -> ("("^string_of_bool a ^ ", " ^ string_of_bool b^")")) lst) ^ "]"
let string_of_bool_list_list lst_lst =
  "[" ^ String.concat "; " (List.map string_of_bool_list lst_lst) ^ "]"
let string_of_2_bool_list_list lst_lst =
  "[" ^ String.concat "; " (List.map string_of_2_bool_list lst_lst) ^ "]"
*)
