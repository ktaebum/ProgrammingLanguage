let sumprod ((f, n, k): ((int * int -> float) * int * int)): float =
  let rec rec_sumprod (i: int): float =

  (* recursive product function *)
  let rec fixed_i ((ii, jj): (int * int)):float =
    if jj = k 
    then begin
      f (ii, k)
    end
    else f (ii, jj) *. fixed_i (ii, jj + 1) in

  if i = n 
  then begin 
    fixed_i (n, 1) 
  end
  else begin
    fixed_i (i, 1) +. rec_sumprod (i + 1) 
  end in

  rec_sumprod 1;;
