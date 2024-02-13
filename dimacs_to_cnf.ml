open quine_dpll

 let formula (filename : string) : cnf  =
   let x = open_in filename in
   let (f : cnf ref) = ref [] in
   let b = ref true in
   try
     while !b do
       let line = input_line x in
          match line.[0] with
            | '%'  -> b := false
            | 'p' | 'c' -> () 
            | _ -> let line_list = List.filter (fun s -> s <> "")(String.split_on_char ' ' line) in
                   let literals = List.filter_map (fun s -> int_of_string_opt s) line_list in
                   let (c : clause) =  List.tl(List.rev (List.map litteral_of_int literals)) in f := c :: !f
       done;
     !f;
   with
     |End_of_file -> close_in x;
     !f;;



(*  TESTS   *)
let formule = formula "uf20-91/uf20-05.cnf";;
dpll formule;; 

let s = " -1 2 3 0";;
let b = List.filter (fun s -> s <> "")(String.split_on_char ' ' s);;
let literals = List.filter_map (fun a -> int_of_string_opt a) b;;
let c = List.tl (List.rev (List.map litteral_of_int literals));;

let sudoku = formula "sudoku44.cnf";;
dpll sudoku;;
quine sudoku;;
