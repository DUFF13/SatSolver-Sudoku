


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


let int_of_liiteral (l : litteral) : int = 
  match l with
  | Var n -> n
  | NVar n -> -n;;

let litteral_of_int (n : int): litteral = 
  if n < 0 then NVar(-n) else Var(n);;

let neg_of_litteral (l : litteral) : litteral = 
  match l with
  | Var x -> NVar x
  | NVar x -> Var x;;

let rec print_list (l : 'a list) = 
  match l with
  | [] -> ()
  | t :: q -> print_string(t); print_string " "; print_list q;;

let rec empty_clause_in (f : cnf) : bool = 
  match f with
  | [] -> false
  | t :: q -> (t = [] || empty_clause_in q);;

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
  | t :: q -> clause_without_negx t x :: cnf_without_negx q x
               
          
;;


(* Algorithm Quine + DPLL *) 

let rec quine (f : cnf) : bool =
  match f with
  | [] -> true 
  | _ when empty_clause_in f -> false
  | (Var x :: _) :: _ | (NVar x :: _) :: _ ->
    if quine ((cnf_without_clause_x ((cnf_without_negx f (Var x)))) (Var x))
       then true
    else quine (cnf_without_negx (cnf_without_clause_x f (NVar x)) (NVar x))
  | _ -> failwith "impossible";;



(*  TEST  *)
let (clause1 : clause) = int_list_to_litteral_list [-1];;

let (clause2 : clause) = int_list_to_litteral_list [1];;
let (clause3 : clause) = int_list_to_litteral_list [-1; 2];;

let (f : cnf) = [clause1; clause2];;

cnf_without_clause_x f (Var(1));;
cnf_without_negx f (Var(2));;
quine f;;
