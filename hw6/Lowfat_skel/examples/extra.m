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
  if (x = y) then write(x) else x in
  f true true
end