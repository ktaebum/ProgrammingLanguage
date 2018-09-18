exception InvalidArgument

type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list


let rec diff ((eq, s): (ae * string)):ae =
  match eq with
  | CONST _ -> CONST 0
  | VAR v -> 
    if v = s then CONST 1
    else CONST 0
  | POWER (v, p) -> 
    if (v <> s) then CONST 0 (* cannot diff *)
    else begin
      if p = 0
      then 
        (* same as const *)
        CONST 0
      else if p = 1
      then CONST 1
      else if p = 2
      then TIMES ((CONST p) :: [VAR v])
      else TIMES ((CONST p) :: [POWER (v, p - 1)])
    end
  | TIMES seq -> 
    if seq = [] then raise InvalidArgument
    else begin
      let rec diffTIMES (l1: ae list) (l2: ae list) (fIdx: int):ae list list =
        let rec diffTIMES_ (l: ae list) (skipIdx: int) (curIdx: int): ae list =
            match l with
            | [] -> []
            | hd :: tl ->
              if skipIdx = curIdx then (diffTIMES_ tl skipIdx (curIdx + 1))
              else hd :: (diffTIMES_ tl skipIdx (curIdx + 1))
        in
        match l1 with
        | [] -> []
        | hd :: tl -> ((diff (hd, s)) :: (diffTIMES_ l2 fIdx 0)) 
                      :: (diffTIMES tl l2 (fIdx + 1))
      in
      let unfolded = (diffTIMES seq seq 0) in
      let rec wrapUp (l: ae list list): ae list =
        match l with 
        | [] -> []
        | hd :: tl -> (TIMES hd) :: (wrapUp tl)
      in
      SUM (wrapUp unfolded)
    end
  | SUM seq -> 
    let rec diffSUMList (l:ae list):ae list =
      match l with 
      | [] -> []
      | hd :: tl -> (diff (hd, s)) :: (diffSUMList tl)
    in
    if seq = [] then raise InvalidArgument
    else SUM (diffSUMList seq)


let test1 () =
  let first = SUM [POWER ("x", 3); VAR "b"] in
  let second = SUM [ TIMES [CONST 2; POWER ("x", 2)]; VAR "a"] in
  let exp = TIMES [first; second] in

  diff (exp, "x");;


let test2()=
  let first = SUM [VAR "x"; VAR "b"] in
  let second = SUM [VAR "c"; VAR "x"] in
  let third = TIMES [first; second] in

  diff (third, "x");;

let test3() =
  diff (SUM ([TIMES [CONST 5; TIMES([VAR "x";VAR "x"])]; CONST 1]), "x")

let test4() = 
  diff (TIMES [CONST 5; TIMES([VAR "x";VAR "x"])], "x")
let test5() = 
  diff (TIMES([VAR "x";VAR "x"]), "x")

let test6() = 
  diff (TIMES [CONST 5; TIMES [VAR "x"; VAR "x"]], "x")

let test7() = 
  diff ( SUM[CONST 1; VAR "x"; VAR "y"; VAR "z"], "asdf")

let test8() = 
  diff (SUM [ TIMES [VAR "x"; VAR "y"; ]; TIMES [VAR "y"; POWER ("x", 6) ] ],
          "y")

let test9() = 
  diff (TIMES [ POWER( "x", 6); VAR "y"], "y")
