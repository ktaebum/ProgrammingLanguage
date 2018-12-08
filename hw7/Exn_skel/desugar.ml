(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)


(* Key: Selective CPS Transformation *)
open Xexp

(* Magic Number *)
let unHandled = 201812

let kcount = ref 0
let hcount = ref 0
let count = ref 0

let new_name () = 
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

let newHVar() = 
  let _ = hcount := !hcount + 1 in
  "h_" ^ (string_of_int !hcount)

let newKVar() = 
  let _ = kcount := !kcount + 1 in
  "k_" ^ (string_of_int !kcount)

(* Got from HW6 CPS_Skel *)
let rec alpha_conv exp subst = 
  match exp with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subst) with Not_found -> Var x)
  | Fn (x, e) ->
    let x' = new_name () in
    let subst' = (x, x') :: subst in
    Fn (x', alpha_conv e subst')
  | App (e1, e2) -> App (alpha_conv e1 subst, alpha_conv e2 subst)
  | If (e1, e2, e3) -> 
    If (alpha_conv e1 subst, alpha_conv e2 subst, alpha_conv e3 subst)
  | Equal (e1, e2) ->
    Equal (alpha_conv e1 subst, alpha_conv e2 subst)
  | Raise e ->
    Raise (alpha_conv e subst)
  | Handle (e1, n, e2) ->
    Handle (alpha_conv e1 subst, n, alpha_conv e2 subst)

(* For Debugging *)
let rec xexp2string: xexp -> string = fun e ->
  match e with 
  | Num n -> "[Num] " ^ (string_of_int n)
  | Var x -> "[Var] " ^ x
  | Fn (x, e) -> "[Fun] " ^ x ^ " of " ^ (xexp2string e)
  | App (e1, e2) -> "[App] ( " ^ (xexp2string e1) ^ " ), ( " ^ (xexp2string e2) ^ " )"
  | If (e1, e2, e3) -> "[If] " ^ (xexp2string e1) ^ " " ^ (xexp2string e2) ^ " " ^ (xexp2string e3)
  | Equal (e1, e2) -> "[Equal] " ^ (xexp2string e1) ^ " " ^ (xexp2string e2)
  | Raise e -> "[Raise] " ^ (xexp2string e)
  | Handle (e1, n, e2) -> "[Handle] " ^ (xexp2string e1) ^ " for " ^ (string_of_int n) ^ " with " ^ (xexp2string e2)


(* 
Based on prof's 1998 paper,
"Assessing the Overhead of ML Exceptions by Selective CPS Transformation"
  - This doubleCPS function is implementation of Figure 6. Naive CPS transformation T in paper
*)
let rec doubleCPS: xexp -> xexp = fun exp ->
  (* Normal Continuation *)
  let k = newKVar() in
  (* Handler Continuatin *)
  let h = newHVar() in

  match exp with
  (* Do not need handler continuation for constants*)
  | Num n ->
    Fn (k, Fn (h, App (Var k, Num n))) 
  | Var x ->
    Fn (k, Fn (h, App (Var k, Var x))) 
  | Fn (x, e) ->
    Fn (k, Fn (h, App (Var k, Fn (x, doubleCPS e))))

  (* If statement *)
  | If (e1, e2, e3) ->
    let v = new_name() in

    (* else *)
    let e3Block = 
      App (App(doubleCPS e3, Var k), Var h)
    in

    (* then *)
    let e2Block = 
      App (App (doubleCPS e2, Var k), Var h)
    in

    Fn (k,
        Fn (h,
            App (
              App (doubleCPS e1,
                   Fn (v,
                       If (Var v, e2Block, e3Block)
                      )
                  )
            (* when e1 raise error, go to handler immediately *)
            ,Var h
            )
           )
       )
  | Equal (e1, e2) ->
    let v1 = new_name() in 
    let v2 = new_name() in
    Fn (k,
        Fn (h, 
            App (
              App (
                doubleCPS e1,
                Fn (v1,
                    App (
                      App (doubleCPS e2,
                           Fn (v2, 
                               App (Var k, Equal (Var v1, Var v2))
                              )
                          ),
                      (* when e2 raise error, go to handler immediately *)
                      Var h
                    )
                   )
              ),
              (* when e1 raise error, go to handler immediately *)
              Var h
            )
           )
       )

  (* App *)
  | App (e1, e2) ->
    let v1 = new_name() in
    let v2 = new_name() in
    (* Since expression is too complicated, make building blocks *)
    (* Innermost *)
    let block1 = 
      App (App (App (Var v1, Var v2), Var k), Var h) 
    in

    (* Next Innermost which use block1 *)
    let block2 =
      App (App (doubleCPS e2, Fn (v2, block1)), Var h)
    in

    (* Final *)
    let block3 = 
      App (App (doubleCPS e1, Fn (v1, block2)), Var h)
    in

    Fn (k,
        Fn (h,
            block3
           )
       )

  | Raise e ->
    (* Apply handler continuation in normal continuation *)
    Fn (k,
        Fn (h,
            App (App (doubleCPS e, Var h), Var h)
           )
       )

  | Handle (e1, n, e2) ->
    let v = new_name() in

    let block1 = 
      App (App (doubleCPS e2, Var k), Var h) 
    in

    Fn (k,
        Fn (h,
            App (
              App (doubleCPS e1, Var k),
              Fn (v,
                  If (Equal (Var v, Num n),
                      block1,
                      (* pass e1's result to handler continuation *)
                      App (Var h, Var v)
                     )
                 )
            )
           )
       )

(* TODO : Implement this function *)
let removeExn : xexp -> xexp = fun e ->
  (* Identity function for normally exit *)
  let normalEnd = Fn ("@id", Var "@id") in
  (* Error code for exceptional exit (const function) *)
  let errorEnd = Fn ("@err", Num unHandled) in

  let cpsTransformed = doubleCPS (alpha_conv e []) in
  (* Apply *)
  App (App (cpsTransformed, normalEnd), errorEnd)