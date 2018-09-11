type nat = ZERO
         | SUCC of nat;;


let rec natadd ((x, y): (nat * nat)):nat =
  match (x, y) with (ZERO, ZERO) -> ZERO
  | (ZERO, _) -> y
  | (_, ZERO) -> x
  | (SUCC xx, SUCC yy) -> SUCC (SUCC (natadd (xx, yy)));;

let rec natmul ((x, y): (nat * nat)):nat=
  (* 
   * 1. ZERO * _ = ZERO
   * 2. _ * ZERO = ZERO
   * 3. ONE (SUCC ZERO) * _ = _
   * 4. _ * ONE (SUCC ZERO) = _
   * 5.
   *  For example, 5 * 10 can be converted to 
   *  5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5 + 5
   *  , add 5 for 10 times or
   *  10 + 10 + 10 + 10 + 10
   *  , add 10 for 5 times
   *
   *  So, we can use natadd in natmul
   *)
  match (x, y) with 
  | (ZERO, _) -> ZERO
  | (_, ZERO) -> ZERO
  | (SUCC ZERO, SUCC _) -> y
  | (SUCC _, SUCC ZERO) -> x
  | (SUCC xx, _) -> natadd (y, (natmul (xx, y)));;

(*
(* Here comes Tester *)
let rec number2nat (n:int):nat = 
  if n = 0
  then ZERO
  else SUCC (number2nat (n - 1));;


let rec nat2number (n:nat):int =
  match n with
  | ZERO -> 0
  | SUCC nn -> 1 + nat2number nn;;


open Random;;
open Printf;;

let test_sum () = 
  for i = 1 to 100 do
    let x = Random.int 10 in
    let y = Random.int 10 in

    let natsum = natadd ((number2nat x), (number2nat y)) in
    if (x + y) = (nat2number natsum)
    then begin
      Printf.printf "Sum Test %d Success\n" i
    end
    else begin
      Printf.eprintf "Error!\n"
    end
  done;;

let test_mul () = 
  for i = 1 to 100 do
    let x = Random.int 10 in
    let y = Random.int 10 in

    let natprod = natmul ((number2nat x), (number2nat y)) in
    if (x * y) = (nat2number natprod)
    then begin
      Printf.printf "Multiply Test %d Success\n" i
    end
    else begin
      Printf.eprintf "Error!\n"
    end
  done;;


test_sum ();;
test_mul ();;
*)
