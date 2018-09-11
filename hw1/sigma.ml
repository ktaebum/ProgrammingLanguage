let rec sigma ((a, b, f): (int * int *(int->int))): int = 
  if a > b then 0 (* from spec *)
  else if a = b then f a
  else (* a < b, normal case *)
    f a + sigma (a + 1, b, f);;


(*
(* Tester *)
let identity (x:int):int = x;;
let square (x:int):int = x * x;;
let my_func (x:int):int = 4 * (square x) + 2 * x;;

(* get 3 command line argument *)
let test () =
  let a = int_of_string (Sys.argv.(1)) in
  let b = int_of_string (Sys.argv.(2)) in
  let i = int_of_string (Sys.argv.(3)) in
  if i = 0
  then print_endline (string_of_int (sigma (a, b, identity)))
  else if i = 1
  then print_endline (string_of_int (sigma (a, b, square)))
  else print_endline (string_of_int (sigma (a, b, my_func)));;


test();;
*)
