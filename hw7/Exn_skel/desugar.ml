(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp

let unHandled = 201812

type env = (int * xexp) list

let rec stringfyExceptions l = 
  (
    match l with 
    | [] -> ""
    | hd :: tl -> 
      (string_of_int (fst hd)) ^ ", " ^ (stringfyExceptions tl)
  )

let rec xexp2string: xexp -> string = fun e ->
  match e with 
  | Num n -> "[Num] " ^ (string_of_int n)
  | Var x -> "[Var] " ^ x
  | Fn (x, e) -> "[Fun] " ^ x ^ " with " ^ (xexp2string e)
  | App (e1, e2) -> "[App] " ^ (xexp2string e1) ^ " " ^ (xexp2string e2)
  | If (e1, e2, e3) -> "[If] " ^ (xexp2string e1) ^ " " ^ (xexp2string e2) ^ " " ^ (xexp2string e3)
  | Equal (e1, e2) -> "[Equal] " ^ (xexp2string e1) ^ " " ^ (xexp2string e2)
  | Raise e -> "[Raise] " ^ (xexp2string e)
  | Handle (e1, n, e2) -> "[Handle] " ^ (xexp2string e1) ^ " for " ^ (string_of_int n) ^ " with " ^ (xexp2string e2)



let rec desugarize: env -> xexp -> xexp = fun exceptions e ->
  let _ = Printf.printf "%s with <env> [ %s ]\n\n" (xexp2string e) (stringfyExceptions exceptions) in
  match e with 
  | Num n -> Num n
  | Var x -> Var x
  | Fn (x, e) -> 
    let e' = desugarize exceptions e in
    Fn (x, e')
  | App (e1, e2) -> 
    let e1' = desugarize exceptions e1 in
    let e2' = desugarize exceptions e2 in 
    App (e1', e2')
  | If (e1, e2, e3) -> 
    let e1' = desugarize exceptions e1 in
    let e2' = desugarize exceptions e2 in
    let e3' = desugarize exceptions e3 in
    If (e1', e2', e3')
  | Equal (e1, e2) ->
    let e1' = desugarize exceptions e1 in
    let e2' = desugarize exceptions e2 in 
    Equal (e1', e2')
  | Raise e -> 
    (* find exceptions *)
    (*
    let errorCode = desugarize exceptions e in
    (
      match errorCode with
      | Num n -> Num (findException exceptions n)
      | _ -> Num unHandled
    )
    *)
    let e' = desugarize exceptions e in
    e'
  (* If (Equal (e', Num (fst (List.hd exceptions))), (snd (List.hd exceptions)), Num unHandled) *)
  | Handle (e1, n, e2) -> 
    let e1' = desugarize ((n, e2) :: exceptions) e1 in
    e1'
    (*
    let e2' = desugarize exceptions e2 in
    let rec stringfyExceptions l = 
      (
        match l with 
        | [] -> ""
        | hd :: tl -> 
          (string_of_int (fst hd)) ^ ", " ^ (stringfyExceptions tl)
      )
    in
    let _ = Printf.printf "hey %s\n" (stringfyExceptions exceptions) in
    (* let _ = Printf.printf "%d\n" n in *)
    If (Equal (e1', Num n), e2', Num unHandled)
    *)


(* TODO : Implement this function *)
let removeExn : xexp -> xexp = fun e ->
  desugarize [] e