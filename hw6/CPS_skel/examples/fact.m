(rec fac x => (ifzero x then 1 else (x + fac (x + (-1))))) 4

(* (ifzero ((fn x => x + x + x) 2)  then 1 else 2) *)

(* (fn x => (ifzero x then (0) else (x + x + x))) ((fn x => x + 1) 0) *)
