(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
  (* Primitive Data Type *)
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    (* | K.UNIT -> [] *)
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]

  (* Simple Arithmetic *)
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT (e) -> trans e @ [Sm5.NOT]

  (* Binding *)
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.LETF (f, x, e1, e2) ->
      [Sm5.PUSH (Sm5.Fn (x, [Sm5.BIND f] @
                        trans e1 @
                        [Sm5.UNBIND; Sm5.POP]
                        )); Sm5.BIND f] @
      trans e2 @
      [Sm5.UNBIND; Sm5.POP]

  (* I/O *)
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE e -> trans e @ [Sm5.PUT]

  (* Variable Related *)
    | K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.ASSIGN (x, e) -> 
      trans e @
      [Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]

  (* Execution *)
    | K.SEQ (e1, e2) -> trans e1 @ trans e2

  (* Call Function *)
    | K.CALLV (f, arg) ->
      (* arg is expression *)
      [Sm5.PUSH (Sm5.Id f)] @ 
      [Sm5.PUSH (Sm5.Id f)] @ 
      trans arg @
      [Sm5.MALLOC; Sm5.CALL]
    | K.CALLR (f, arg) ->
      (* arg is variable address *)
      [Sm5.PUSH (Sm5.Id f)] @
      [Sm5.PUSH (Sm5.Id f)] @
      [Sm5.PUSH (Sm5.Id arg); Sm5.LOAD] @
      [Sm5.PUSH (Sm5.Id arg)] @
      [Sm5.CALL]

  (* Conditional *)
    | K.IF (cond, trueE, falseE) ->
      trans cond @
      [Sm5.JTR (trans trueE, trans falseE)]


  (* Loop *)
    | K.FOR (i, e1, e2, e) ->
      let anonymous_func = "#f" in
      let functionBody = 
        [Sm5.PUSH (Sm5.Id i); Sm5.LOAD] @
        trans (K.ADD (e2, K.NUM 1)) @
        [Sm5.LESS] @
        [Sm5.JTR (trans e, trans K.UNIT)] @

        (* increment i *)
        [Sm5.JTR (trans e @
                  (* Increment i *)
                  [Sm5.PUSH (Sm5.Id i); Sm5.LOAD] @
                  [Sm5.PUSH (Sm5.Val (Sm5.Z 1))] @
                  [Sm5.ADD] @
                  [Sm5.PUSH (Sm5.Id i); Sm5.STORE] @
                  (* call for loop again *)
                  trans (K.CALLR (anonymous_func, i)),
                  trans K.UNIT)]
      in
      [Sm5.PUSH (Sm5.Fn (i, [Sm5.BIND anonymous_func] @
                            functionBody @
                            [Sm5.UNBIND; Sm5.POP])); Sm5.BIND anonymous_func] @
      trans e1 @
      trans (K.ADD (e2, K.NUM 1)) @
      [Sm5.LESS] @
      [Sm5.JTR (trans e1 @
                [Sm5.PUSH (Sm5.Id i); Sm5.STORE] @
                trans (K.CALLR (anonymous_func, i)), trans K.UNIT)] @ 
      [Sm5.UNBIND; Sm5.POP]
    | K.WHILE (cond, e) ->
      trans cond @
      [Sm5.JTR (trans e @
                trans (K.WHILE (cond, e)),
                trans K.UNIT)]
      


end
