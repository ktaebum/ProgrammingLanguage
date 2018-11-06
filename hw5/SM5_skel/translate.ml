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
    | K.WRITE e -> 
      (* trans e should calculate integer *)
      let anonymousFunc = "#f" in
      let anonymousArg = "#arg" in
      let functionBody = 
        trans e @
        trans e
      in
      (* trans e @ [Sm5.PUT] *)
      
      (*
      [Sm5.PUSH (Sm5.Fn (anonymousArg, [Sm5.BIND anonymousFunc] @
                            functionBody @
                            [Sm5.UNBIND; Sm5.POP])); Sm5.BIND anonymousFunc] @
      *)
      let rec afterPUT e = 
        (match e with 
         | K.WRITE e' -> afterPUT e'
         | K.SEQ (e1, e2) ->
           (match (e1, e2) with
            | (K.WRITE e1', K.WRITE e2') -> K.SEQ (afterPUT e1', afterPUT e2')
            | (_, K.WRITE e2') -> K.SEQ (e1, afterPUT e2')
            | (K.WRITE e1', _) -> K.SEQ (afterPUT e1', e2)
            | _ -> K.SEQ (e1, e2)
           )
         | _ -> e
        )
      in
      trans e @
      (* trans (K.CALLV (anonymousFunc, e)) @ *)
      [Sm5.PUT]
      (* @ trans (afterPUT e) *)

      (* [Sm5.UNBIND; Sm5.POP] *)
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
      let anonymousFunc = "#f" in
      let functionBody = 
        (* increment i *)
        trans e @
        [Sm5.PUSH (Sm5.Id i); Sm5.LOAD] @
        [Sm5.PUSH (Sm5.Val (Sm5.Z 1))] @
        [Sm5.ADD] @

        (* compare with e2 *)
        trans (K.ADD (e2, K.NUM 1)) @
        [Sm5.LESS] @
        [Sm5.JTR (
            (* assign and call function again *)
            [Sm5.PUSH (Sm5.Id i); Sm5.LOAD] @
            [Sm5.PUSH (Sm5.Val (Sm5.Z 1))] @
            [Sm5.ADD] @
            [Sm5.PUSH (Sm5.Id i); Sm5.STORE] @
            trans (K.CALLR (anonymousFunc, i)),
            trans K.UNIT)]
      in
      [Sm5.PUSH (Sm5.Fn (i, [Sm5.BIND anonymousFunc] @
                            functionBody @
                            [Sm5.UNBIND; Sm5.POP])); Sm5.BIND anonymousFunc] @
      trans e1 @
      trans (K.ADD (e2, K.NUM 1)) @
      [Sm5.LESS] @
      [Sm5.JTR (trans e1 @
                [Sm5.PUSH (Sm5.Id i); Sm5.STORE] @
                trans (K.CALLR (anonymousFunc, i)), trans K.UNIT)] @ 
      [Sm5.UNBIND; Sm5.POP]
    | K.WHILE (cond, e) ->
      let anonymousFunc = "#f" in
      let anonymousArg = "#arg" in
      let functionBody = 
        trans e @
        trans cond @
        [Sm5.JTR (
            trans (K.CALLR (anonymousFunc, anonymousArg)),
            trans K.UNIT)]
      in
      [Sm5.PUSH (Sm5.Val (Sm5.Z 0))] @
      [Sm5.MALLOC; Sm5.BIND anonymousArg; Sm5.PUSH (Sm5.Id anonymousArg); Sm5.STORE] @
      [Sm5.PUSH (Sm5.Fn (anonymousArg, [Sm5.BIND anonymousFunc] @
                            functionBody @
                            [Sm5.UNBIND; Sm5.POP])); Sm5.BIND anonymousFunc] @
      trans cond @
      [Sm5.JTR (trans (K.CALLR (anonymousFunc, anonymousArg)),
                trans K.UNIT)] @
      [Sm5.UNBIND; Sm5.POP] @
      [Sm5.UNBIND; Sm5.POP]
      


end
