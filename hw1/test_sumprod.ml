#use "topfind";;
#require "sumprod";;
open Sumprod;;
open Printf;;

let f1 ((x, y): (int * int)): float =
  (float_of_int x) *. (float_of_int y);;

let f2 ((x, y): (int * int)): float =
  let fx:float = float_of_int x in
  let fy:float = float_of_int y in
  fx *. fx +. fy *. fy;;

let f3 ((x, y): (int * int)): float =
  let fx:float = float_of_int x in
  let fy:float = float_of_int y in
  4. *. (fx *. fx) +. 10. *. fx -. 8. *. fy +. (fy *. fy *. fy);;

(* get 3 command line argument *)
let test () =
  let n = int_of_string (Sys.argv.(1)) in
  let k = int_of_string (Sys.argv.(2)) in
  let i = int_of_string (Sys.argv.(3)) in
  if i = 0
  then Printf.printf "%.5f\n" (sumprod (f1, n, k))
  else if i = 1
  then Printf.printf "%.5f\n" (sumprod (f2, n, k))
  else begin
    Printf.printf "%.5f\n" (sumprod (f3, n, k))
  end;;

test();;


