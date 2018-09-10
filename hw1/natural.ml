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
