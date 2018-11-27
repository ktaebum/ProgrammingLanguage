let val print2 = fn x => 				
    let val a = x.1 
        val b = x.2
    in
        write a; write b
    end
  in
  print2 (2, 3)
end
