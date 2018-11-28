(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0
let wcount = ref 0
let ecount = ref 0


(* For easy debugging, seperate var *)
let new_var () =
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)
let new_evar () =
  let _ = ecount := !ecount +1 in
  "e_" ^ (string_of_int !count)
let new_wvar () =
  let _ = wcount := !wcount +1 in
  "w_" ^ (string_of_int !count)

type typ = 
  | TInt
  | TBool
  | TString 
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | TEq of var (* equivalent *)
  | TWr of var (* writable *)
(* Modify, or add more if needed *)

let newVar () = 
  let v = new_var() in
  TVar v
let newEVar () = 
  let v = new_evar() in
  TEq v
let newWVar () = 
  let v = new_wvar() in
  TWr v

(* For Debugging *)
let  rec typ2string: typ -> string = fun typ ->
  match typ with
  | TInt -> "Int"
  | TBool -> "Bool"
  | TString -> "String"
  | TLoc l -> "Location of (" ^ (typ2string l) ^ ")"
  | TVar v -> "Unknown variable " ^ v
  | TEq v -> "Unknown eq variable " ^ v
  | TWr v -> "Unknown write variable " ^ v
  | TFun (arg, ret) -> "Function of (" ^ (typ2string arg) ^ " -> " ^ (typ2string ret) ^ ")"
  | TPair (t1, t2) -> "Pair of (" ^ (typ2string t1) ^ ", " ^ (typ2string t2) ^ ")"


(* Below helper functions are got from previous year's course material *)
(* Gamma (type env) related *)
(* type gamma = M.id -> typ *)
type gamma = (M.id * typ) list
(* bind x -> t into gamma *)
let (@+) gamma (x, t) = fun y -> if (y = x) then t else gamma y
let emptyGamma = (fun x -> raise (M.TypeError ("Not binded variable" ^ x)))
let find env id = 
  (* return typ of given id *)
  try
    let (_, t) = List.find (fun v -> (fst v) = id) env in
    t
  with Not_found -> raise (M.TypeError ("Not binded variable" ^ id))


(* Substitution Related 
   Substitution: function that substitutes type variable into real type
*)
type subst = typ -> typ
let emptySubst: subst = fun t -> t
let makeSubst: var -> typ -> subst = fun x t ->
  (* Make substitution of Var x -> type t *)
  let rec subs t' = 
    match t' with
    | TVar x' -> 
      (* if x' is x, our target 
         Since prefix of TWr and TEq are different, it is safe matching
         Substitute TVar x' as t*)
      if (x = x') then t else t'
    | TWr w' ->
      if (x = w') then t else t'
    | TEq e' ->
      if (x = e') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt
    | TBool
    | TString -> t'
  in subs
(* Merge two substitution *)
let (@@) s1 s2 = (fun t -> s1 (s2 t)) 
let substEnv : subst -> gamma -> gamma = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subs tyscm)) tyenv


(* TODO:
    Make unification algorithm

    Reference 
    http://www.cs.cornell.edu/courses/cs3110/2011fa/supplemental/lec26-type-inference/type-inference.htm
    Class Material 5-1.pdf

    Unification Rule
    unification (var, t) =  var -> t
    unification (t, var) = var -> t
    unification (Loc t1, Loc t2) = unification t1 t2
    unification (Pair (t1, t2), Pair (t3, t4)) = 
      unification t1 t3 && unification t2 t4 && merge subst
*)

let rec hasAlpha: var -> typ -> bool = fun alpha tau ->
  (* Check occurence of alpha \in tau *)
  match tau with 
  | TVar x -> x = alpha
  | TWr w -> w = alpha
  | TEq w -> w = alpha
  | TPair (t1, t2) 
  | TFun (t1, t2) ->
    (hasAlpha alpha t1) || (hasAlpha alpha t2)
  | TString
  | TBool
  | TInt -> false
  | TLoc t -> hasAlpha alpha t



let rec unification: typ -> typ -> subst = fun t1 t2 ->
  (* get two typ and return subst *)
  if (t1 = t2) then emptySubst
  else (
    match (t1, t2) with 
    | (TVar alpha, tau)  -> 
      if (hasAlpha alpha tau) then raise (M.TypeError "alpha in tau")
      else makeSubst alpha tau
    | (tau, TVar alpha)  -> 
      if (hasAlpha alpha tau) then raise (M.TypeError "alpha in tau")
      else makeSubst alpha tau
    | (TWr alpha, tau)  -> 
      (
        match tau with 
        | TBool
        | TString
        | TInt
        | TWr _ -> makeSubst alpha tau
        | TEq v -> makeSubst v (TWr alpha) (* Narrow scope *)
        | _ -> raise (M.TypeError "Cannot write")
      )
    | (tau, TWr alpha)  -> 
      (
        match tau with 
        | TBool
        | TString
        | TInt
        | TWr _ -> makeSubst alpha tau
        | TEq v -> makeSubst v (TWr alpha) (* Narrow scope *)
        | _ -> raise (M.TypeError "Cannot write")
      )
    | (tau, TEq alpha)  -> 
      (
        match tau with 
        | TBool
        | TString
        | TInt
        | TWr _
        | TEq _ -> makeSubst alpha tau
        | TLoc ltype -> 
          if (hasAlpha alpha ltype) then raise (M.TypeError "alpha in tau")
          else makeSubst alpha tau
        | _ -> raise (M.TypeError "Cannot Compare Equality")
      )
    | (TEq alpha, tau)  -> 
      (
        match tau with 
        | TBool
        | TString
        | TInt
        | TWr _
        | TEq _ -> makeSubst alpha tau
        | TLoc ltype -> 
          if (hasAlpha alpha ltype) then raise (M.TypeError "alpha in tau")
          else makeSubst alpha tau
        | _ -> raise (M.TypeError "Cannot Compare Equality")
      )
    | (TLoc lt1, TLoc lt2) -> unification lt1 lt2
    | (TFun (pt1, pt2), TFun (pt3, pt4))
    | (TPair (pt1, pt2), TPair (pt3, pt4)) ->
      let s1 = unification pt1 pt3 in
      let s2 = unification pt2 pt4 in
      s1 @@ s2
    | _ -> raise (M.TypeError "Unification failed")
  )


(* TODO:
   Make type equation construction algorithm
   Reference course material 5-1.pdf

   Based on W algorithm
*)
let rec eval: (gamma * M.exp) -> (typ * subst) = fun (env, exp) ->
  match exp with
  (* Const *)
  | M.CONST M.N _ -> (TInt, emptySubst)
  | M.CONST M.S _ -> (TString, emptySubst)
  | M.CONST M.B _ -> (TBool, emptySubst)

  (* Variable *)
  | M.VAR id -> (find env id, emptySubst)

  (* Function *)
  | M.FN (x, e) ->
    (* Unknown argument x type *)
    let alpha = newVar() in

    (* Evaluate type e == returnType 
     * argumentType will be evaluated in this statement and stored in s
    *)
    let (tau, s) = eval (((x, alpha) :: env), e) in

    (TFun (s alpha, tau), s)

  | M.APP (e1, e2) ->
    let (tau, s) = eval (env, e1) in
    let (tau', s') = eval (substEnv s env, e2) in

    (* Unknown return type *)
    let alpha = newVar() in
    let s'' = unification (TFun (tau', alpha)) (s' tau) in

    (s'' alpha, s'' @@ s' @@ s)

  (* BOP *)
  | M.BOP (M.ADD, e1, e2)
  | M.BOP (M.SUB, e1, e2) ->
    (* Operand must be int pair  *)
    let equalArg = TPair (TInt, TInt) in

    let (tau, s) = eval (env, e1) in
    let (tau', s') = eval (substEnv s env, e2) in
    let s'' = unification equalArg (TPair (s tau, s' tau')) in
    (TInt, s'' @@ s' @@ s)
  | M.BOP (M.OR, e1, e2)
  | M.BOP (M.AND, e1, e2) ->
    (* Same as *)
    let equalArg = TPair (TBool, TBool) in

    let (tau, s) = eval (env, e1) in
    let (tau', s') = eval (substEnv s env, e2) in
    let s'' = unification equalArg (TPair (s tau, s' tau')) in
    (TBool, s'' @@ s' @@ s)
  | M.BOP (M.EQ, e1, e2) -> 
    (* Both type should be same into alpha*) 
    let alpha = newEVar() in

    (* Eval left operand *)
    let (tau, s) = eval (env, e1) in

    (* Substitute alpha into tau *)
    let s' = unification alpha tau in

    (* From known result, eval right operand *)
    let (tau', s'') = eval (substEnv (s' @@ s) env, e2) in

    (* Two types must be same *)
    let s''' = unification (s' tau) tau' in
    (TBool, s''' @@ s'' @@ s' @@ s)

  (* Let Binding *)
  | M.LET ((M.VAL (x, e1)), e2) ->
    (* Refer to https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Let-polymorphism *)
    let (tau, s) = eval (env, e1) in
    let env' = substEnv s env in
    let (tau', s') = eval ((x, tau)::env', e2) in


    (s' tau', s' @@ s)

  | M.LET ((M.REC (f, x, e1)), e2) ->
    (* funtion f: alpha -> beta *)
    (* arg x : alpha *)
    let alpha = newVar() in
    let beta = newVar() in
    let env' = (x, alpha) :: env in
    let ftype = TFun (alpha, beta) in
    let env'' = (f, ftype) :: env' in

    (* evalulate function body 
       this tau is a return type *)
    let (tau, s) = eval (env'', e1) in 
    let s' = unification ftype (TFun (s alpha, s tau)) in
    let env' = (f, (TFun (s' alpha, s' tau))) :: (x, s' alpha) :: env in
    let (tau', s'') = eval (env', e2) in
    (s'' tau', s'' @@ s' @@ s)

  (* Memory *)
  | M.MALLOC e ->
    (* Must be location *)
    let (tau, s) = eval (env, e) in
    (TLoc (s tau), s)
  | M.ASSIGN (e1, e2) ->
    let (tau, s) =  eval (env, e1) in
    let (tau', s') = eval (env, e2) in
    (
      match tau with
      | TLoc ltype ->(
          let s'' = unification ltype tau' in
          (s'' tau', s'' @@ s' @@ s)
        )
      | TVar v ->
        (* tau must be a location type *)
        let alpha = newVar() in
        let s'' = unification tau (TLoc alpha) in
        let s''' = unification (s'' alpha) tau' in
        (s''' tau', s''' @@ s'' @@ s' @@ s)
      | _ -> raise (M.TypeError "Assign to non-loc type")
    )
  | M.BANG e ->
    (* Invariant: e must be a location *)
    let (tau, s) = eval (env, e) in
    (
      match tau with 
      | TLoc locType -> (s locType, s)
      | TVar v ->
        let alpha = newVar() in
        let s' = unification tau (TLoc alpha) in
        (s' alpha, s' @@ s)
      | _ -> raise (M.TypeError "BANG only location")
    )

  (* Control Statement *)
  | M.IF (cond, e1, e2) ->
    (* Must be a boolean *)
    let (tau, s) = eval (env, cond) in
    let s' = unification tau TBool in

    (* eval then and else *)
    let (tau1, s1) = eval (substEnv s env, e1) in
    let (tau2, s2) = eval (substEnv (s1 @@ s) env, e2) in

    (* Must be same *)
    let s'' = unification tau1 tau2 in

    (* Return *)
    (s'' tau2, s''  @@ s2 @@ s1 @@ s' @@ s)

  (* I/O *)
  | M.READ -> (TInt, emptySubst)
  | M.WRITE e ->
    let (tau, s) = eval (env, e)  in
    (
      match tau with
      | TInt
      | TBool
      | TWr _  (* Keep it *)
      | TString -> (tau, s)
      | TVar _ ->
        let alpha = newWVar() in
        let s' = unification tau alpha in
        (s' alpha, s' @@ s)
      | TEq _ ->
        let alpha = newWVar() in
        let s' = unification tau alpha in
        (s' alpha, s' @@ s)
      | _ -> raise (M.TypeError "Invalid write type")
    )

  (* Pair *)
  | M.PAIR (e1, e2) ->
    let (tau, s) = eval (env, e1) in
    let (tau', s') = eval (env, e2) in
    (* subst pair *)
    ((s'@@ s) (TPair (s tau, s' tau')), s' @@ s)
  | M.FST (e) ->
    let alpha = newVar() in
    let beta = newVar() in
    let (tau, s) = eval (env, e) in
    (* tau must be a pair *)
    let s' = unification (TPair (alpha, beta)) (s tau) in
    (* let _ = Printf.printf "(FST) %s\n" (typ2string (TPair ((s@@s') alpha, (s@@s') beta))) in *)
    (s' alpha, s' @@ s)
  | M.SND (e) ->
    let alpha = newVar() in
    let beta = newVar() in
    let (tau, s) = eval (env, e) in
    (* tau must be a pair *)
    let s' = unification (TPair (alpha, beta)) (s tau) in
    (s' beta, s' @@ s)
  | M.SEQ (e1, e2) ->
    let (tau, s) = eval (env, e1) in
    let (tau', s') = eval ((substEnv s env), e2) in
    (s' tau', s' @@ s)



let rec typ2types: typ -> M.types = fun typ ->
  match typ with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (typ2types t1, typ2types t2)
  | TLoc (t) -> M.TyLoc (typ2types t)
  | TVar x -> raise (M.TypeError ("invalid result: type var " ^ x))
  | TWr x -> raise (M.TypeError ("invalid result: write type var " ^ x))
  | TEq x -> raise (M.TypeError ("invalid result: eq type var " ^ x))
  | TFun _ -> raise (M.TypeError "invalide result: type fucntion")


(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp ->
  let (result, _) = eval ([], exp) in
  typ2types result
