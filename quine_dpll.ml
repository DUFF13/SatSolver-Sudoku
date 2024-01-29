open Sat_solver

(* Algorithm : Quine + DPLL *) 

let rec quine (f : cnf) : bool * clause =
  match f with
  | [] -> true, []
  | _ when empty_clause_in f -> (false, [])
  | (x :: _) :: _ ->
  let (sat, c) = quine ((cnf_without_clause_x ((cnf_without_negx f x))) x) in
    if sat then (sat, x :: c)
    else let (sat, c) = quine (cnf_without_negx (cnf_without_clause_x f (neg_of_litteral x)) (neg_of_litteral x)) in
         if not sat then (false, []) else (sat, neg_of_litteral x :: c) 
  | _ -> failwith "impossible";;



let rec dpll (f : cnf) : bool * clause = (* problem *)
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
let (clause2 : clause) = int_list_to_litteral_list [1; -2;];;
let (clause3 : clause) = int_list_to_litteral_list [1; 2];;
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


let input_cnf = [
    [Var 16; Var 30; Var 95];
    [NVar 16; Var 30; Var 95];
    [NVar 30; Var 35; Var 78];
    [NVar 30; NVar 78; Var 85];
    [NVar 78; NVar 85; Var 95];
    [Var 8; Var 55; Var 100];
    [Var 8; Var 55; NVar 95];
    [Var 9; Var 52; Var 100];
    [Var 9; NVar 73; NVar 100];
    [NVar 8; NVar 9; Var 52];
    [Var 40; NVar 55; Var 90];
    [NVar 40; NVar 55; Var 90];
    [Var 25; Var 35; Var 82];
    [NVar 25; Var 82; NVar 90];
    [NVar 55; NVar 82; NVar 90];
    [Var 11; Var 75; Var 84];
    [Var 11; NVar 75; Var 96];
    [NVar 11; Var 23; NVar 35];
    [NVar 23; Var 29; Var 65];
    [Var 29; NVar 35; NVar 65];
    [NVar 23; NVar 29; Var 84];
    [NVar 35; Var 54; Var 70];
    [NVar 54; Var 70; Var 77];
    [NVar 19; NVar 77; NVar 84];
    [NVar 19; NVar 54; Var 70];
    [Var 22; Var 68; Var 81];
    [NVar 22; Var 48; Var 81];
    [NVar 22; NVar 48; Var 93];
    [NVar 3; NVar 48; NVar 93];
    [Var 7; NVar 18; NVar 81];
    [NVar 7; Var 56; NVar 81];
    [Var 3; NVar 18; Var 56];
    [NVar 18; Var 47; Var 68];
    [NVar 18; NVar 47; NVar 81];
    [NVar 3; Var 68; Var 77];
    [NVar 3; NVar 77; NVar 84];
    [NVar 19; Var 68; NVar 70];
    [NVar 19; NVar 68; Var 74];
    [NVar 68; NVar 70; NVar 74];
    [Var 54; Var 61; NVar 62];
    [Var 50; Var 53; NVar 62];
    [NVar 50; Var 61; NVar 62];
    [NVar 27; Var 56; Var 93];
    [Var 4; Var 14; Var 76];
    [Var 4; NVar 76; Var 96];
    [NVar 4; Var 14; Var 80];
    [NVar 14; NVar 68; Var 80];
    [NVar 10; NVar 39; NVar 89];
    [Var 1; Var 49; NVar 81];
    [Var 1; Var 26; NVar 49];
    [NVar 17; NVar 26; NVar 49];
    [NVar 1; NVar 17; NVar 40];
    [Var 16; Var 51; NVar 89];
    [NVar 9; Var 57; Var 60];
    [Var 12; Var 45; NVar 51];
    [Var 2; Var 12; Var 69];
    [Var 2; NVar 12; NVar 40];
    [NVar 12; NVar 51; Var 69];
    [NVar 33; Var 60; NVar 98];
    [NVar 5; NVar 32; NVar 66];
    [Var 2; NVar 47; NVar 100];
    [NVar 42; Var 64; Var 83];
    [Var 20; NVar 42; NVar 64];
    [Var 20; NVar 48; Var 98];
    [NVar 20; Var 50; Var 98];
    [NVar 32; NVar 50; Var 98];
    [NVar 24; NVar 37; Var 73];
    [NVar 24; NVar 37; NVar 100];
    [NVar 57; Var 71; Var 81];
    [NVar 37; Var 40; NVar 91];
    [Var 31; Var 42; Var 81];
    [NVar 31; Var 42; Var 72];
];;

dpll input_cnf;;
quine input_cnf;;

let formula_test = [[Var 16; Var 17; Var 30];
    [NVar 17; Var 22; Var 30];
    [NVar 17; NVar 22; Var 30];
    [Var 16; NVar 30; Var 47];
    [Var 16; NVar 30; NVar 47];
    [NVar 16; NVar 21; Var 31];
    [NVar 16; NVar 21; NVar 31];
    [NVar 16; Var 21; NVar 28];
    [NVar 13; Var 21; Var 28];
    [Var 13; NVar 16; Var 18];
    [Var 13; NVar 18; NVar 38];
    [Var 13; NVar 18; NVar 31];
    [Var 31; Var 38; Var 44];
    [NVar 8; Var 31; NVar 44];
    [Var 8; NVar 12; NVar 44];
    [Var 8; Var 12; NVar 27];
    [Var 12; Var 27; Var 40];
    [NVar 4; Var 27; NVar 40];
    [Var 12; Var 23; NVar 40];
    [NVar 3; NVar 4; NVar 23];
    [NVar 3; NVar 23; NVar 49];
    [NVar 3; NVar 13; NVar 49];
    [NVar 23; NVar 26; Var 49];
    [Var 12; NVar 34; Var 49];
    [NVar 12; Var 26; NVar 34];
    [Var 19; Var 34; Var 36];
    [NVar 19; Var 26; Var 36];
    [NVar 30; Var 34; NVar 36];
    [Var 24; Var 34; NVar 36];
    [NVar 24; NVar 36; Var 43];
    [Var 6; Var 42; NVar 43];
    [NVar 24; Var 42; NVar 43];
    [NVar 5; NVar 24; NVar 42];
    [Var 5; Var 20; NVar 42];
    [Var 5; NVar 7; NVar 20];
    [Var 4; Var 7; Var 10];
    [NVar 4; Var 10; NVar 20];
    [Var 7; NVar 10; NVar 41];
    [NVar 10; Var 41; Var 46];
    [NVar 33; Var 41; NVar 46];
    [Var 33; NVar 37; Var 46];
    [Var 32; Var 33; Var 37];
    [NVar 6; Var 32; Var 37];
    [NVar 6; Var 25; NVar 32];
    [NVar 6; NVar 25; NVar 48];
    [NVar 9; Var 28; NVar 48];
    [NVar 9; NVar 25; NVar 28];
    [Var 19; NVar 25; Var 48];
    [Var 2; Var 9; NVar 19];
    [NVar 2; NVar 19; Var 35];
    [NVar 2; Var 22; NVar 35];
    [NVar 22; NVar 35; Var 50];
    [NVar 17; NVar 35; NVar 50];
    [NVar 29; NVar 35; NVar 50];
    [NVar 1; NVar 29; NVar 50];
    [Var 1; Var 11; Var 29];
    [NVar 11; Var 17; NVar 45];
    [NVar 11; Var 39; Var 45];
    [NVar 26; Var 39; Var 45];
    [NVar 3; NVar 26; Var 45];
    [NVar 11; Var 15; NVar 39];
    [Var 14; NVar 15; NVar 39];
    [Var 14; NVar 15; NVar 45];
    [Var 14; NVar 15; NVar 27];
    [NVar 14; NVar 15; Var 47];
    [Var 17; Var 17; Var 40];
    [Var 1; NVar 29; NVar 31];
    [NVar 7; Var 32; Var 38];
    [NVar 14; NVar 33; NVar 47];
    [NVar 1; Var 2; NVar 8];
    [Var 35; Var 43; Var 44];
    [Var 21; Var 21; Var 24];
    [Var 20; Var 29; NVar 48];
    [Var 23; Var 35; NVar 37];
    [Var 2; Var 18; NVar 33];
    [Var 15; Var 25; NVar 45];
    [Var 9; Var 14; NVar 38];
    [NVar 5; Var 11; Var 50];
    [NVar 3; NVar 13; Var 46];
    [NVar 13; NVar 41; Var 43]];;

dpll formula_test;;
quine formula_test;;
