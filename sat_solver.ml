
(* SAT SOLVER USING QUINE AND DPLL ALGORITHMS, DPLL IS JUST AN OPTIMIZE QUINE ALGORITHM *)
(* All useful type declaration *)

type litteral =
  |Var of int
  |NVar of int
;;

type clause = litteral list;;
type cnf = clause list;;


(* Basic function *)

let rec size_list (l: 'a list) = 
  match l with
  | [] -> 0;
  | _ :: q -> 1 + size_list q;;


let int_of_litteral (l : litteral) : int = 
  match l with
  | Var n -> n
  | NVar n -> -n;;

let litteral_of_int (n : int): litteral = 
  if n < 0 then NVar(-n) else Var(n);;

let neg_of_litteral (l : litteral) : litteral = 
  match l with
  | Var x -> NVar x
  | NVar x -> Var x;;

let rec print_clause (l : 'a list) = 
  match l with
  | [] -> ()
  | t :: q -> print_int(int_of_litteral t); print_string " "; print_clause q;;

let rec print_cnf (f : cnf) =
  match f with
  | [] -> ()
  | t :: q -> print_string "("; print_clause t; print_string ") ^ "; print_cnf q;;

let rec print_list (l : 'a list) : unit =
  match l with
  | [] -> ()
  | t :: q -> print_string(t); print_string " "; print_list q;;

let rec empty_clause_in (f : cnf) : bool = List.mem [] f

let int_list_to_litteral_list (l : int list) = List.map litteral_of_int l;;


let rec cnf_without_clause_x (f : cnf)(x : litteral) : cnf =
  match f with
  | [] -> []
  | t :: q when not(List.mem x t) -> t ::  cnf_without_clause_x q x
  | _ :: q -> cnf_without_clause_x q x;;

let rec clause_without_negx (c : clause)(x : litteral) : clause =
  match c with
  | [] -> []
  | t :: q when t <> neg_of_litteral x -> t :: clause_without_negx q x
  | _ :: q -> clause_without_negx q x;;

let rec cnf_without_negx (f : cnf)(x : litteral) : cnf =
  match f with
  | [] -> []
  | t :: q -> clause_without_negx t x :: cnf_without_negx q x;;

let rec one_var_clause (f : cnf) : bool * litteral =
  match f with
  | [] -> false, Var 0
  | t :: q -> match t with  
              | [] -> false, Var 0  (* with dpll or quine t cannot be empty but compiler not happy non-exhaustiv pattern-matching *)
              | a :: [] -> (true, a)
              | _ :: b -> one_var_clause q;;

let rec clause_without_x (f : clause)(x : litteral) : clause =
  match f with
  | [] -> []
  | t :: q -> if (t = x || t = neg_of_litteral x) then clause_without_x q x
              else t :: clause_without_x q x;;


let pur_var_cnf (f : cnf): bool * litteral =
  let rec aux (g : clause) : bool * litteral =
    match g with
    | [] -> false, Var 0 (* with dpll or quine t cannot be empty*)
    | t :: q when not (List.mem (neg_of_litteral t) q) -> (true, t)
    | t :: q -> aux (clause_without_x q t)
  in aux (List.flatten f);;


(* Algorithm : Quine + DPLL *) 

(* let rec quine (f : cnf) : bool * clause =
  match f with
  | [] -> true, []
  | _ when empty_clause_in f -> (false, [])
  | (Var x :: _) :: _ | (NVar x :: _) :: _ ->
  let (sat, c) = quine ((cnf_without_clause_x ((cnf_without_negx f (Var x)))) (Var x)) in
    if sat then (sat, Var x :: c)
    else let (sat, c) = quine (cnf_without_negx (cnf_without_clause_x f (NVar x)) (NVar x)) in
         if not sat then (false, []) else (sat, NVar x :: c) 
  | _ -> failwith "impossible";;



let rec dpll (f : cnf) : bool * clause =
  match f with
  | [] -> (true, [])
  | t  :: q when empty_clause_in f -> (false, [])
  | (x :: _ ) :: _ | (x :: _) :: _ ->
  let (one, lit) = one_var_clause f in
  if one then let (sat, c) = dpll ((cnf_without_clause_x ((cnf_without_negx f (lit)))) (lit)) in
              if sat then (sat, lit :: c)
              else let (sat, c) = dpll (cnf_without_negx (cnf_without_clause_x f (neg_of_litteral lit)) (neg_of_litteral lit)) in
                   if not sat then (false, []) else (sat, neg_of_litteral lit :: c)
  else let (pur, lit) = pur_var_cnf f in
       if pur then let (sat, c) = dpll (cnf_without_negx (cnf_without_clause_x f (neg_of_litteral lit)) (neg_of_litteral lit)) in
                   if not sat then (false, []) else (sat, neg_of_litteral lit :: c)
  else let (sat, c) = dpll ((cnf_without_clause_x ((cnf_without_negx f x))) x) in
              if sat then (sat, x :: c)
              else let (sat, c) = dpll (cnf_without_negx (cnf_without_clause_x f x) x) in
                   if not sat then (false, []) else (sat, neg_of_litteral x :: c)
  |_ -> failwith "impossible"





(*  TEST  *)
let (clause1 : clause) = int_list_to_litteral_list [-1; 2];;
let (clause2 : clause) = int_list_to_litteral_list [1; -2;-3];;
let (clause3 : clause) = int_list_to_litteral_list [-3; 2];;
let (f : cnf) = [clause1; clause2; clause3];;
let a = List.flatten f;;
clause_without_x a (Var 2);;
one_var_clause f;;
pur_var_cnf f;;
print_cnf f;;
let cnf_satisfiable = [[Var 1; Var 2]; [NVar 1; Var 2; NVar 3]; [Var 3; NVar 2]; [Var 1; NVar 3]];;
let cnf_insatisfiable = [[Var 1; Var 2]; [NVar 1; NVar 2]; [Var 1; NVar 2]; [NVar 1; Var 2]];;
let cnf_satisfiable_values = [[NVar 1; NVar 2; Var 3];  [NVar 1; NVar 2; NVar 3]; [NVar 1; Var 2; Var 3]];;
let cnf_satisfiable_values_2 = [[Var 1; NVar 2; NVar 3]; [NVar 1; NVar 2; NVar 3]; [Var 1; Var 3; NVar 4]; [NVar 2]];;
cnf_without_clause_x f (Var(1));;
cnf_without_negx f (Var(2));;
quine cnf_satisfiable;;
quine cnf_satisfiable_values_2;;
quine cnf_insatisfiable;;
quine f;;
dpll f;;
dpll cnf_satisfiable;;
dpll cnf_satisfiable_values;;
dpll cnf_satisfiable_values_2;;
dpll cnf_insatisfiable;;

*)
