exception FreeVariable


type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

let rec calculate (e: exp):float =
  (* In here, do not interpret type X as polynomial *)

  let rec assign (x:exp) (v:float):float = 
    (* Assign given value to expression
     * It interpret type X as polynomial function
     *)
    match x with
    | X -> v
    | INT i -> float_of_int i
    | REAL f -> f
    | ADD (e1, e2) -> (assign e1 v) +. (assign e2 v)
    | SUB (e1, e2) -> (assign e1 v) -. (assign e2 v)
    | MUL (e1, e2) -> (assign e1 v) *. (assign e2 v)
    | DIV (e1, e2) -> (assign e1 v) /. (assign e2 v)
    | _ -> 
      (* rest of case can be handled in calculate 
       * Since it is either SIGMA or INTEGRAL *)
      calculate x
  in
  match e with
  | X -> raise FreeVariable
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (calculate e1) +. (calculate e2)
  | SUB (e1, e2) -> (calculate e1) -. (calculate e2)
  | MUL (e1, e2) -> (calculate e1) *. (calculate e2)
  | DIV (e1, e2) -> (calculate e1) /. (calculate e2)
  | SIGMA (e1, e2, e3) -> 
    if (calculate e1) > (calculate e2)
    then 0.
    else begin
      let a = int_of_float (calculate e1) in
      let b = int_of_float (calculate e2) in  
      if a > b
      then 0.
      else (assign e3 (float_of_int a)) +. (calculate (SIGMA (INT (a + 1), INT b, e3)))
    end
  | INTEGRAL (e1, e2, e3) -> 1.
