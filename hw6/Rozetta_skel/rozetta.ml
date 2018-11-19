(*
 * SNU 4190.310 Programming Languages 
 * Homework "Rozetta" Skeleton
 *)

(* For anonymous function *)
let continuationFunc = "#contFunc"
let continuationArg = "#contArg"
let continuationVar = "#contVar"
let continuationOffset = 10
let continuationTime = ref 0

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
    (* current command is Sm5.cmd list. convert it into Sonata cmd list*)
    let command = 
      [Sonata.PUSH (Sonata.Id arg)] @ (* get location of arg *)
      [Sonata.PUSH (Sonata.Val (Sonata.Z continuationOffset))] @
      [Sonata.ADD] @
      [Sonata.LOAD] @
      [Sonata.BIND continuationFunc] @
      (trans' command) @
      [Sonata.PUSH (Sonata.Id continuationFunc)] @
      [Sonata.UNBIND] @
      [Sonata.POP] @
      [Sonata.PUSH (Sonata.Val (Sonata.Z 0))] @
      [Sonata.MALLOC] @
      [Sonata.CALL] in
    Sonata.Fn (arg, command)

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds)
  | Sm5.JTR (c1, c2) :: cmds ->  [Sonata.JTR (trans' c1, trans' c2)]
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds)
  | Sm5.GET ::cmds -> Sonata.GET :: (trans' cmds)
  | Sm5.PUT ::cmds -> Sonata.PUT :: (trans' cmds)
  | Sm5.CALL :: cmds -> 
    (*
     * Invariant: Stack is composed of
     *  l :: v :: (x, C' E') :: (x, C', E') :: S 
     *
     * View (C, E) as new function
     *)
    let getContinuation = 
      if (!continuationTime == 0) then
        (* Main function *)
        let _ = continuationTime := 1 in
        Sonata.Fn (continuationArg, trans' cmds)
      else
        (* Not main function *)
    in

    let _ = if (cmds <> []) then continuationTime := !continuationTime + 1 in
    let _ = Printf.printf "cont = %d\n" !continuationTime in
    let anonymousArg = "#temp" in
    let continuation = Sonata.Fn (continuationArg, trans' cmds) in
    [Sonata.BIND anonymousArg] @ 
    [Sonata.PUSH (Sonata.Id anonymousArg)] @
    [Sonata.PUSH continuation] @
    [Sonata.PUSH (Sonata.Id anonymousArg)] @
    (* Now the stack becomes l :: f :: l :: v :: (x, C', E') :: (x, C', E') :: S *)

    (* Set offset *)
    [Sonata.PUSH (Sonata.Val (Sonata.Z continuationOffset))] @
    [Sonata.ADD] @
    (* Now the stack becomes 
     * l + offset :: f :: l :: v :: (x, C', E') :: (x, C' E') :: S *)

    [Sonata.STORE] @
    (* Now the stack becomes l :: v :: (x, C', E') :: (x, C', E') :: S 
     * In memory, l + offset has continuation function*)

    [Sonata.UNBIND] @
    [Sonata.POP] @

    [Sonata.CALL]

  (* | Sm5.CALL :: cmds -> failwith "TODO : fill in here" *)
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
  trans' command
