(*
 * SNU 4190.310 Programming Languages 
 * Homework "Rozetta" Skeleton
 *)

(* For anonymous function *)
let continuationArg = "#contArg"
let previousCont = "#previousCont" 
let functionCall = "#functionCall"


let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z  -> Sonata.Z z
  | Sm5.B b  -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

(* TODO : complete this function *)
let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, command) -> 
    let command = 
      (* execute real function Body *)
      (trans' command) @

      (* decrement function call *)
      [Sonata.PUSH (Sonata.Id functionCall)] @
      [Sonata.LOAD] @
      [Sonata.PUSH (Sonata.Val (Sonata.Z 1))] @
      [Sonata.SUB] @
      [Sonata.PUSH (Sonata.Id functionCall)] @
      [Sonata.STORE] @

      (* load previous Cont *)
      [Sonata.PUSH (Sonata.Id previousCont)] @
      [Sonata.PUSH (Sonata.Id functionCall)] @
      [Sonata.LOAD] @
      [Sonata.ADD] @
      [Sonata.LOAD] @

      (* call previous conv *)
      [Sonata.PUSH (Sonata.Val (Sonata.Z 0))] @
      [Sonata.MALLOC] @
      [Sonata.CALL] in
    (* let _ = continuationCount := !continuationCount - 1 in *)
    (* let _ = Printf.printf "at fn cont = %d\n" !continuationCount in *)
    Sonata.Fn (arg, command)

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds)
  | Sm5.JTR (c1, c2) :: cmds ->  Sonata.JTR (trans' c1 @ trans' cmds, trans' c2 @ trans' cmds) :: []
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds)
  | Sm5.GET ::cmds -> Sonata.GET :: (trans' cmds)
  | Sm5.PUT ::cmds -> Sonata.PUT :: (trans' cmds)
  | Sm5.CALL :: cmds -> 
    (*
     * These commands run in environment E 
     *)

    (* 
     * Invariant: Stack is composed of
     *  l :: v :: (x, C' E') :: (x, C', E') :: S 
     *
     * View (C, E) as a new function
     *)

    let baseContinuation = Sonata.Fn (continuationArg, 
                                      [Sonata.UNBIND] @
                                      [Sonata.POP] @
                                      trans' cmds) in
    let prevContinuation = Sonata.Fn (continuationArg, 
                                      [Sonata.UNBIND] @
                                      [Sonata.POP] @
                                      trans' cmds @
                                      (* decrement function call *)
                                      [Sonata.PUSH (Sonata.Id functionCall)] @
                                      [Sonata.LOAD] @
                                      [Sonata.PUSH (Sonata.Val (Sonata.Z 1))] @
                                      [Sonata.SUB] @
                                      [Sonata.PUSH (Sonata.Id functionCall)] @
                                      [Sonata.STORE] @

                                      (* load next continuation *)
                                      [Sonata.PUSH (Sonata.Id previousCont)] @
                                      [Sonata.PUSH (Sonata.Id functionCall)] @
                                      [Sonata.LOAD] @
                                      [Sonata.ADD] @
                                      [Sonata.LOAD] @
                                      [Sonata.PUSH (Sonata.Val (Sonata.Z 0))] @
                                      [Sonata.MALLOC] @
                                      [Sonata.CALL] 
                                     ) in
    (* load functionCall *)
    [Sonata.PUSH (Sonata.Id functionCall)] @
    [Sonata.LOAD] @

    (* check whether this is main function or not *)
    [Sonata.PUSH (Sonata.Val (Sonata.Z 1))] @
    [Sonata.LESS] @
    [Sonata.JTR ([Sonata.PUSH baseContinuation], [Sonata.PUSH prevContinuation])] @
    [Sonata.PUSH (Sonata.Id previousCont)] @
    [Sonata.PUSH (Sonata.Id functionCall)] @
    [Sonata.LOAD] @
    [Sonata.ADD] @
    [Sonata.STORE] @
    
    (* increment functionCall *)
    [Sonata.PUSH (Sonata.Id functionCall)] @
    [Sonata.LOAD] @
    [Sonata.PUSH (Sonata.Val (Sonata.Z 1))] @
    [Sonata.ADD] @
    [Sonata.PUSH (Sonata.Id functionCall)] @
    [Sonata.STORE] @

    [Sonata.CALL]

  | Sm5.ADD :: cmds -> Sonata.ADD :: (trans' cmds)
  | Sm5.SUB :: cmds -> Sonata.SUB :: (trans' cmds)
  | Sm5.MUL :: cmds -> Sonata.MUL :: (trans' cmds)
  | Sm5.DIV :: cmds -> Sonata.DIV :: (trans' cmds)
  | Sm5.EQ :: cmds -> Sonata.EQ :: (trans' cmds)
  | Sm5.LESS :: cmds -> Sonata.LESS :: (trans' cmds)
  | Sm5.NOT :: cmds -> Sonata.NOT :: (trans' cmds)
  | [] -> []

(* TODO : complete this function *)
let trans : Sm5.command -> Sonata.command = fun command ->

  (* for counting function call depth (recursive call or nested function) *)
  [Sonata.MALLOC] @
  [Sonata.BIND functionCall] @
  [Sonata.PUSH (Sonata.Val (Sonata.Z 0))] @
  [Sonata.PUSH (Sonata.Id functionCall)] @
  [Sonata.STORE] @

  (* Bind previousCont variable and store empty function *)
  [Sonata.MALLOC] @
  [Sonata.BIND previousCont] @
  [Sonata.PUSH (Sonata.Fn (continuationArg, []))] @
  [Sonata.PUSH (Sonata.Id previousCont)] @
  [Sonata.STORE] @
  trans' command @
  [Sonata.UNBIND] @
  [Sonata.POP] @
  [Sonata.UNBIND] @
  [Sonata.POP]
