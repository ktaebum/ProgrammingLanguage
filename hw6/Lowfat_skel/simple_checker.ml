(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0

let new_var () =
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)

(* For Debugging *)
let rec typ2string t =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TPair (t1, t2) -> "Pair (" ^ (typ2string t1) ^ ", " ^ (typ2string t2) ^ ")"
  | TLoc t1 -> "Loc " ^ (typ2string t1)
  | TFun (t1, t2) -> "Fun " ^ (typ2string t1) ^ " -> " ^ (typ2string t2)
  | TVar v -> "Var " ^ v


(* Setup Gamma *)
type gamma = var -> typ
let bindType gamma (x, t) =
  (* at gamma g, convert all occurence of x into t *)
  fun y -> if (y) = (x) then t else gamma y
let lookup gamma x = gamma x
let emptyGamma = (fun x -> raise (M.RunError ("unbound id to type " ^ x)))

let rec exp2typ (gamma: gamma) (exp: M.exp) =
  match exp with
  (* CONST *)
  | M.CONST M.S _ -> (gamma, TString)
  | M.CONST M.B _ -> (gamma, TBool)
  | M.CONST M.N _ -> (gamma, TInt)
  | M.VAR v -> (gamma, gamma v)

  (* Operations *)
  | M.BOP (M.EQ, e1, e2) ->
    let (gamma', type1) = exp2typ gamma e1 in
    let (gamma'', type2) = exp2typ gamma' e2 in
    (
      match (type1, type2) with
      | (TVar x1, TVar x2) ->
        (* Keep variable as variable
         * At this case, both variables can be
         *  String
         *  Boolean
         *  Integer
         *  Location
         *)
        (* let gamma'' = bindType gamma'' (x1, TAlpha) in *)
        (* let gamma'' = bindType gamma'' (x2, TAlpha) in *)
        (gamma'', TBool)
      | (TVar x1, TInt) -> (bindType gamma'' (x1, TInt), TBool)
      | (TVar x1, TString) -> (bindType gamma'' (x1, TString), TBool)
      | (TVar x1, TBool) -> (bindType gamma'' (x1, TBool), TBool)
      | (TVar x1, TLoc ltype) -> (bindType gamma'' (x1, TLoc ltype), TBool)
      | (TInt, TVar x2) -> (bindType gamma'' (x2, TInt), TBool)
      | (TString, TVar x2) -> (bindType gamma'' (x2, TString), TBool)
      | (TBool, TVar x2) -> (bindType gamma'' (x2, TBool), TBool)
      | (TLoc ltype, TVar x2) -> (bindType gamma'' (x2, TLoc ltype), TBool)
      | (TInt, TInt)
      | (TBool, TBool)
      | (TString, TString)
        (* TODO:
         * Does recursive check for pointer needed?
         *)
      | (TLoc _, TLoc _) -> (gamma'', TBool)
      | (_, _) -> raise (M.TypeError "Invalid equivalent compare")
    )
  | M.BOP (M.OR, e1, e2)
  | M.BOP (M.AND, e1, e2) ->
    let (gamma', type1) = exp2typ gamma e1 in
    let (gamma'', type2) = exp2typ gamma' e2 in
    (
      match (type1, type2) with
      | (TVar x1, TVar x2) ->
        (* Variable must be boolean *)
        let gamma'' = bindType gamma'' (x1, TBool) in
        let gamma'' = bindType gamma'' (x2, TBool) in
        (gamma'', TBool)
      | (TVar x1, TBool) -> (bindType gamma'' (x1, TBool), TBool)
      | (TBool, TVar x2) -> (bindType gamma'' (x2, TBool), TBool)
      | (TBool, TBool) -> (gamma'', TBool)
      | (_, _) -> raise (M.TypeError "OR/AND operation type mismatch")
    )
  | M.BOP (M.ADD, e1, e2)
  | M.BOP (M.SUB, e1, e2) ->
    (* valid iff both types are TyInt *)
    let (gamma', type1) = exp2typ gamma e1 in
    let (gamma'', type2) = exp2typ gamma' e2 in
    (
      match (type1, type2) with
      | (TVar x1, TVar x2) -> 
        (* Variable must be integer *)
        let gamma'' = bindType gamma'' (x1, TInt) in
        let gamma'' = bindType gamma'' (x2, TInt) in
        (gamma'', TInt)
      | (TVar x1, TInt) -> (bindType gamma'' (x1, TInt), TInt)
      | (TInt, TVar x2) -> (bindType gamma'' (x2, TInt), TInt)
      | (TInt, TInt) -> (gamma'', TInt)
      | (_, _) -> raise (M.TypeError "ADD/SUB operation type mismatch")
    )

  (* Control Statement *)
  | M.IF (e1, e2, e3) ->
    let (gamma', type1) = exp2typ gamma e1 in
    if (type1 = TBool) then (
      let (gamma'', type2) = exp2typ gamma' e2 in
      let (gamma''', type3) = exp2typ gamma'' e3 in
      if (type2 = type3) then (gamma''', type2)
      else raise (M.TypeError "Type mismatch in if statement branch")
    )
    else raise (M.TypeError "Condition of if statement is not a Bool")

  (* Function Related *)
  | M.APP (e1, e2) ->
    let (gamma', type1) = exp2typ gamma e1 in
    (match type1 with
     | TFun (t1, t2) ->
       (
         match (t1, t2) with
         | (TVar x, TVar y) -> 
           (* it is same as alpha -> alpha *)
           if (x = y) then exp2typ gamma' e2
           else raise (M.TypeError "Function App type mismatch")
         | (TVar x, _) -> (gamma', t2)
         | _ -> 
           let (gamma'', type2) = exp2typ gamma' e2 in
           if (type2 = t1) then (gamma'', t2)
           else raise (M.TypeError "Function App type mismatch")
       )
     | _ -> raise (M.TypeError "First type of APP must be function")
    )
    
  | M.FN (x, e) ->
    let gamma' = bindType gamma (x, TVar x) in
    let (gamma', etype) = exp2typ gamma' e in

    (* For debugging *)
    (* let _ = Printf.printf "This Function Type is %s -> %s\n" (typ2string (gamma' x)) (typ2string etype) in *)

    (gamma', TFun (gamma' x, etype))
  | M.LET ((M.VAL (x, e1)), e2) ->
    let (gamma', etype) = exp2typ gamma e1 in

    (* For debugging *)
    let _ = Printf.printf "Expression type is %s\n" (typ2string etype) in

    let gamma' = bindType gamma' (x, etype) in
    exp2typ gamma' e2
  | M.LET ((M.REC (f, x, e1)), e2) ->
    (*
     * 1. BIND f that it takes x as arg
     *)
    let gamma' = bindType gamma (f, TFun (TVar x, TVar x)) in
    let gamma' = bindType gamma' (x, TVar x) in
    let (gamma', etype) = exp2typ gamma' e1 in
    let gamma' = bindType gamma' (x, etype) in

    (* For debugging *)
    let _ = Printf.printf "Expression type is %s\n" (typ2string (gamma' f)) in

    exp2typ gamma' e2

  (* I/O Related *)
  | M.READ -> (gamma, TInt)
  | M.WRITE e ->
    let (gamma', type1) = exp2typ gamma e in
    if ((type1 = TInt) || (type1 = TBool) || (type1 = TString)) then (gamma', type1)
    else raise (M.TypeError "Write only support Int, Bool, and String")

  (* Memory Related *)
  | M.MALLOC e ->
    let (gamma', type1) = exp2typ gamma e in
    (gamma', TLoc type1)
  | M.ASSIGN (e1, e2) ->
    let (gamma', type1) = exp2typ gamma e1 in
    let (gamma'', type2) = exp2typ gamma' e2 in
    (
      match type1 with
      | TLoc ltype -> 
        if (type2 = ltype) then (gamma'', type2)
        else raise (M.TypeError "Assign Failure (assign type mismatch)")
      | _ -> raise (M.TypeError "Assign Failure (not a location)")
    )
  | M.BANG e ->
    let (gamma', type1) = exp2typ gamma e in
    (
      match type1 with
      | TLoc ltype -> (gamma', ltype)
      | _ -> raise (M.TypeError "Non-location dereference")
    )

  (* Pair Related *)
  | M.PAIR (e1, e2) ->
    let (gamma', type1) = exp2typ gamma e1 in
    (* For debugging *)
    let _ = Printf.printf "First pair is %s\n" (typ2string type1) in
    let (gamma'', type2) = exp2typ gamma' e2 in
    let _ = Printf.printf "Second pair is %s\n" (typ2string type1) in
    (gamma'', TPair (type1, type2))
  | M.FST e ->
    let (gamma', type1) = exp2typ gamma e in
    (
      match type1 with
      | TPair (t1, _) -> (gamma', t1)
      | _ -> raise (M.TypeError "Not a pair")
    )
  | M.SND e ->
    let (gamma', type1) = exp2typ gamma e in
    (
      match type1 with
      | TPair (_, t2) -> (gamma', t2)
      | _ -> raise (M.TypeError "Not a pair")
    )

  | M.SEQ (e1, e2) -> 
    let (gamma', _ ) = exp2typ gamma e1 in
    exp2typ gamma' e2



let rec typ2types = function
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (typ2types t1, typ2types t2)
  | TLoc (t) -> M.TyLoc (typ2types t)
  | TFun (t1, t2) -> M.TyArrow (typ2types t1, typ2types t2)
  | _ -> raise (M.TypeError "invalid result type")


(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp ->
	let (_, typResult) = exp2typ emptyGamma exp in
	typ2types typResult
