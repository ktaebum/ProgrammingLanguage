(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]

  (* Binding *)
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.LETF (f, x, e1, e2) ->
      [Sm5.PUSH (Sm5.Fn (x, trans e1)); Sm5.BIND f] @
      trans e2 @
      [Sm5.UNBIND; Sm5.POP]

  (* I/O *)
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE e -> trans e @ [Sm5.PUT]

  (* Variable Related *)
    | K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.ASSIGN (x, e) -> 
      trans e @
      [Sm5.PUSH (Sm5.Id x); Sm5.STORE]

  (* Execution *)
    | K.SEQ (e1, e2) -> trans e1 @ trans e2

  (* Call Function *)
    | K.CALLV (f, arg) ->
      [Sm5.PUSH (Sm5.Id f)] @
      trans arg @
      [Sm5.MALLOC; Sm5.CALL]
    | _ -> failwith "Unimplemented"
      


end
