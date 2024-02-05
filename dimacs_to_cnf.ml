open quine_dpll

(* let formula (filename : string) : cnf  = (* fnc * number of variables * number of clauses *)
   let x = open_in filename in
   let (f : cnf ref) = ref [] in
   let b = ref true in
   try
     while !b do
       let line = input_line x in
          match line.[0] with
            | '%' -> b := false
            | 'p' | 'c' | ' ' -> () (* let line_list = List.filter (fun s -> s <> "")(String.split_on_char ' ' line) in
                     print_list line_list;*)
            | _ -> let line_list = List.filter (fun s -> s <> "")(String.split_on_char ' ' line) in
                   let (c : clause) = List.tl (List.rev (List.map litteral_of_int (List.map int_of_string line_list))) in f := c :: !f
       done;
     !f;
   with
     |End_of_file -> close_in x;
     !f;;


let formule = formula "uf20-91/uf20-05.cnf";;
dpll formule;; *)

let read (filename : string) : cnf =
  let li
  let (f : cnf ref) = ref []  in
  let x = open_in filename in
  try
    while true; do
      let line = input_line x in
      let line_list = List.filter (fun s -> s <> "")(String.split_on_char ' ' line) in print_list line_list;
      if ((String.length line > 0) && not (line.[0] = 'c' || line.[0] = 'p')) then
       
    done; !f
  with End_of_file -> close_in x;
 !f;;


let formule1 = formula "uf20-91/uf20-05.cnf";;

let l = ["1"; "3"];;
List.map int_of_string l;;
