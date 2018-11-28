let val f = fn x => fn y =>
  (x.1) = (y.2) in
  f ((10,10), (20, 20)) (20,( 10 ,10))
end
