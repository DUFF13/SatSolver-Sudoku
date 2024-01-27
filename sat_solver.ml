(* All useful type declaration *)

type litteral =
  |Var of int
  |NVar of int
;;

type clause = litteral list;;
type cnf = clause list;;


(* Basic function *)

let rec size_list ('a list) = 
  match l with
  | [] -> 0;
  | _ :: q -> 1 + size q;;


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

let rec empty_clause_in (f : cnf) : bool = List.mem [] f;;

let int_list_to_litteral_list (l : int list) = List.map litteral_of_int l;;


let rec cnf_without_clause_x (f : cnf)(x : litteral) : cnf =
  match f with
  | [] -> []
  | t :: q when not(List.mem x t) -> t ::  cnf_without_clause_x q x
  | _ :: q -> cnf_without_clause_x q x;;


let rec cnf_without_negx (f : cnf)(x : litteral) : cnf =
  match f with
  | [] -> []
  | t :: q ->
    let rec clause_without_negx (c : clause) (x : litteral) : clause =
        match t with
        | [] -> []
        | a :: b when a <> neg_of_litteral x -> a :: clause_without_negx b x
        | _ :: b -> clause_without_negx b x
    in clause_without_negx t x :: cnf_without_negx q x;;
               
          
;;


(* Algorithm Quine + DPLL *) 

let rec quine (f : cnf) : bool =
  match f with
  | [] -> true
  | _ when empty_clause_in f -> false
  | (Var x :: _) :: _ | (NVar x :: _) :: _ ->
    if quine (cnf_without_clause_x (List.filter ()))
    





(*  TEST  *)
let (clause1 : clause) = int_list_to_litteral_list [1; -2];;

let (clause2 : clause) = int_list_to_litteral_list [3; 2];;
let (clause3 : clause) = int_list_to_litteral_list [-1; -2];;

let (f : cnf) = [clause1; clause2; clause3];;
cnf_without_clause_x f (Var(1));;
cnf_without_negx f (Var(2));;

quine f;;
