(*
let val f = fn x =>
  x := 3 in
  write(f (malloc 1))
end
*)

(*
let val f = fn x =>
  !x in
  write (f (malloc true))
end
*)

let val f = fn x => fn y =>
  (
    (x = y);
    x := 3
  )
  in
  f (malloc 3) (malloc 5)
end