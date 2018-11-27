let rec comp = fn a => fn b =>
      if (a + 1) = b then false
      else if (a - 1) = b then true
      else comp (a - 1) b 
in
    if a = b then false
    else comp a b 
end 