(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
 *)

open M

type var = string

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | TWrite of var (* writable var *)
  | TEQ of var (* Equivalent comparable var *)
(* Modify, or add more if needed *)

let rec typ2string: typ -> string = fun t ->
  match t with
  | TInt -> "Int"
  | TBool -> "Bool"
  | TString -> "String"
  | TPair (t1, t2) -> "Pair of (" ^ (typ2string t1) ^ ", " ^ (typ2string t2) ^ ")"
  | TFun (t1, t2) -> "Function of (" ^ (typ2string t1) ^ ", " ^ (typ2string t2) ^ ")"
  | TVar x -> "Unknown type " ^ x
  | TWrite x -> "Unknown writable type " ^ x
  | TEQ x -> "Unknown eq comparable type " ^ x
  | TLoc lt -> "Location of " ^ (typ2string lt)

type typ_scheme =
  | SimpleTyp of typ 
  (* General Type *)
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list
let find env id =
  try
    let (_, scheme) = List.find (fun v -> (fst v) = id) env in
    (
      match scheme with
      | SimpleTyp t -> t
      | GenTyp _ -> failwith "Unimplemented"
    )
  with Not_found -> raise (M.TypeError ("Not bound id " ^ id))

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

let newVar () = 
  (* Wrapper of new_var *)
  let v = new_var() in
  TVar v

let newWVar () =
  let v = new_var() in
  TWrite v

let newEVar () =
  let v = new_var() in
  TEQ v

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 = 
  (* Union free type variable *)
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2

let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  (* typ to free variable list *)
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v -> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left 
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv 

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TWrite x'
    | TEQ x'
    | TVar x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->

  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

(* Unify *)
let rec hasVar: var -> typ -> bool = fun v tau ->
  (* Check occurence of v in tau *)
  match tau with 
  | TVar x
  | TWrite x
  | TEQ x -> x = v
  | TPair (t1, t2)
  | TFun (t1, t2) ->
    (hasVar v t1) || (hasVar v t2)
  | TString
  | TBool
  | TInt -> false
  | TLoc lt -> hasVar v lt and
  unify: typ -> typ -> subst = fun t1 t2 ->
    if (t1 = t2) then empty_subst
    else (
      match (t1, t2) with
      | (TVar v, tau) ->
        if (hasVar v tau) then raise (M.TypeError (v ^ " in " ^ (typ2string tau) ^ ", unsolvable"))
        else make_subst v tau
      | (tau, TVar v) ->
        unify (TVar v) tau
      | (TWrite v, tau) ->
        (
          match tau with
          | TBool
          | TString
          | TInt
          | TWrite _ -> make_subst v tau
          | TEQ v' -> make_subst v' (TWrite v) (* Narrow scope *)
          | _ -> raise (M.TypeError "Cannot write")
        )
      | (tau, TWrite v) ->
        unify (TWrite v) tau
      | (TEQ v, tau) ->
        (
          match tau with
          | TBool
          | TString
          | TInt
          | TWrite _ (* Narrow scope *)
          | TEQ _ -> make_subst v tau
          | TLoc lt ->
            if (hasVar v lt) then raise (M.TypeError (v ^ " in " ^ (typ2string tau) ^ ", unsolvable"))
            else make_subst v tau
          | _ -> raise (M.TypeError ("Not eq comparable type " ^ (typ2string tau)))
        )
      | (tau ,TEQ v) ->
        unify (TEQ v) tau
      | (TLoc lt1, TLoc lt2) -> unify lt1 lt2
      | (TFun (t11, t12), TFun (t21, t22))
      | (TPair (t11, t12), TPair (t21, t22)) ->
        let s1 = unify t11 t21 in
        let s2 = unify t12 t22 in
        s2 @@ s1
      | _ -> raise (M.TypeError ("Unify failed: " ^ (typ2string t1) ^ " and " ^ (typ2string t2)))
    )


(* typ 2 M.types *)
let rec typ2mtyp: typ -> M.typ = fun t ->
  match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (typ2mtyp t1, typ2mtyp t2)
  | TLoc (lt) -> M.TyLoc (typ2mtyp lt)
  | _ -> raise (M.TypeError ("TypeError: invalid result type " ^ (typ2string t)))

(* Based On W Algorithm *)
let rec infer: typ_env -> M.exp -> (subst * typ) = fun env exp ->
  match exp with
  (* Const *)
  | M.CONST M.N _ -> (empty_subst, TInt)
  | M.CONST M.B _ -> (empty_subst, TBool)
  | M.CONST M.S _ -> (empty_subst, TString)
  | M.FN (x, e) ->
    let beta = newVar() in
    let (s1, tau1) = infer ((x, SimpleTyp beta) :: env) e in
    (s1, TFun (s1 beta, tau1))

  (* Var *)
  | M.VAR id -> 
    let tau = find env id in
    (empty_subst, tau)

  (* Let Binding *)
  | M.LET ((M.VAL (x, e1)), e2) ->
    let (s1, tau1) = infer env e1 in
    let env2 = subst_env s1 env in
    let (s2, tau2)  = infer ((x, SimpleTyp tau1) :: env2) e2 in
    (s2 @@ s1, tau2)
  | M.LET ((M.REC (f, x, e1)), e2) ->
    let beta1 = newVar() in
    let beta2 = newVar() in
    let env1 = (x, SimpleTyp beta1) :: env in
    let funcType = TFun (beta1, beta2) in
    let env2 = (f, SimpleTyp funcType) :: env1 in


    let (s1, tau1) = infer env2 e1 in
    let s2 = unify funcType (TFun (s1 beta1, tau1)) in
    let env3 = (f, SimpleTyp (TFun ((s2 beta1), tau1))) :: (x, SimpleTyp (s2 beta1)) :: env in
    let (s3, tau2) = infer env3 e2 in
    (s3 @@ s2 @@ s1, tau2)


  (* APP *)
  | M.APP (e1, e2) ->
    let beta = newVar() in
    let (s1, tau1) = infer env e1 in
    let (s2, tau2) = infer (subst_env s1 env) e2 in
    let s3 = unify (s2 tau1) (TFun (tau2, beta)) in
    (s3 @@ s2 @@ s1, s3 beta)

  (* BOP *)
  | M.BOP (M.ADD, e1, e2)
  | M.BOP (M.SUB, e1, e2) ->
    let argType = TPair (TInt, TInt) in
    let (s1, tau1) = infer env e1 in
    let (s2, tau2) = infer (subst_env s1 env) e2 in
    let s3 = unify argType (TPair (tau1, tau2)) in
    (s3 @@ s2 @@ s1, TInt)

  | M.BOP (M.OR, e1, e2)
  | M.BOP (M.AND, e1, e2) ->
    let argType = TPair (TBool, TBool) in
    let (s1, tau1) = infer env e1 in
    let (s2, tau2) = infer (subst_env s1 env) e2 in
    let s3 = unify argType (TPair (tau1, tau2)) in
    (s3 @@ s2 @@ s1, TBool)
  | M.BOP (M.EQ, e1, e2) ->
    let beta = newVar() in

    (* infer tau1 *)
    let (s1, tau1) = infer env e1 in

    (* tau1 must be eq comparable type *)
    let s2 = unify beta tau1 in

    (* from calculated subst, infer tau2 *)
    let (s3, tau3) = infer (subst_env (s2 @@ s1) env) e2 in

    (* two types must be same *)
    let s4 = unify tau3 (s2 beta) in

    (s4 @@ s3 @@ s2 @@ s1, TBool)

  (* Memory *)
  | M.MALLOC e ->
    let (s1, tau1) = infer env e in
    (s1, TLoc tau1)
  | M.ASSIGN (e1, e2) ->
    let (s1, tau1) = infer env e1 in
    let (s2, tau2) = infer (subst_env s1 env) e2 in
    (
      match tau1 with
      | TLoc lt -> (
          let s3 = unify lt tau2 in
          (s3 @@ s2 @@ s1, tau2)
        )
      | TEQ _ 
      | TVar _ ->
        (* tau1 should be a location type. Narrow scope *)
        let s3 = unify tau1 (TLoc tau2) in
        (s3 @@ s2 @@ s1, tau2)
      | _ -> raise (M.TypeError ("[ASSIGN] Not a location type: " ^ (typ2string tau1)))
    )
  | M.BANG e ->
    let (s1, tau1) = infer env e in
    (
      match tau1 with
      | TLoc lt -> 
        (s1, lt)
      | TEQ _
      | TVar _ ->
        (* tau1 should be a location type. Narrow scope *)
        let beta = newVar() in
        let s2 = unify tau1 (TLoc beta) in
        (s2 @@ s1, s2 beta)
      | _ -> raise (M.TypeError ("[ASSIGN] Not a location type: " ^ (typ2string tau1)))
    )

  (* Control Statement *)
  | M.IF (cond, e1, e2) ->
    let (s1, tau1) = infer env cond in
    let s2 = unify tau1 TBool in

    let (s3, tau3) = infer (subst_env (s2 @@ s1) env) e1 in
    let (s4, tau4) = infer (subst_env (s3 @@ s2 @@ s1) env) e2 in
    let s5 = unify tau3 tau4 in
    (s5 @@ s4 @@ s3 @@ s2 @@ s1, tau4)

  (* I/O *)
  | M.READ -> (empty_subst, TInt)
  | M.WRITE e ->
    let (s1, tau1) = infer env e in
    (
      match tau1 with
      | TInt
      | TBool
      | TWrite _
      | TString -> (s1, tau1)
      | TVar _ 
      | TEQ _ ->
        let beta = newWVar() in
        let s2 = unify tau1 beta in
        (s2 @@ s1, s2 beta)
      | _ -> raise (M.TypeError ("[WRITE] Invalid write type " ^ (typ2string tau1)))
    )

  (* Pair Related *)
  | M.PAIR (e1, e2) ->
    let (s1, tau1) = infer env e1 in
    let (s2, tau2) = infer (subst_env s1 env) e2 in
    (s2 @@ s1, TPair (tau1, tau2))
  | M.FST e ->
    let alpha = newVar() in
    let beta = newVar() in
    let (s1, tau1) = infer env e in
    let s2 = unify (TPair (alpha, beta)) tau1 in
    (s2 @@ s1, s2 alpha)
  | M.SND e ->
    let alpha = newVar() in
    let beta = newVar() in
    let (s1, tau1) = infer env e in
    let s2 = unify (TPair (alpha, beta)) tau1 in
    (s2 @@ s1, s2 beta)
  | M.SEQ (e1, e2) -> 
    let (s1, tau1) = infer env e1 in
    let (s2, tau2) = infer (subst_env s1 env) e2 in
    (s2 @@ s1, tau2)


(* TODO : Implement this function *)
let check : M.exp -> M.typ = fun e ->
  let (s, tau) = infer [] e in
  typ2mtyp tau