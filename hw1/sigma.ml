let rec sigma ((a, b, f): (int * int *(int->int))): int = 
  if a > b then (* prevent infinite loop *)
    let swap ((a, b): (int * int)) = (b, a) in
    let swapped = swap (a, b) in
    sigma (fst swapped, snd swapped, f);
  else if a = b then f a
  else (* a < b, normal case *)
    f a + sigma (a + 1, b, f);;

