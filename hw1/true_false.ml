type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr


let rec eval (f:formula):bool = 
  let rec calculate_expression (e:expr): int = 
    match e with
    | NUM n -> n 
    | PLUS (e1, e2) -> (calculate_expression e1) + (calculate_expression e2)
    | MINUS (e1, e2) -> (calculate_expression e1) - (calculate_expression e2) in
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT sub_f -> not (eval sub_f)
  | ANDALSO (f1, f2) -> 
    if (eval f1) = true
    then
      if (eval f2) = true
      then true
      else false
    else
      (* do not calculate f2 *)
      false
  | ORELSE (f1, f2) -> 
    if (eval f1) = true
    then
      (* do not calculate f2 *)
      true
    else
    if (eval f2) = true
    then
      true
    else
      false
  | IMPLY (f1, f2) -> 
    (* similar to ANDALSO
     * but true when (eval f1) is false *)
    if (eval f1) = true
    then
      if (eval f2) = true
      then true
      else false
    else
      (* do not calculate f2 *)
      true
  | LESS (e1, e2) -> (calculate_expression e1) < (calculate_expression e2)
