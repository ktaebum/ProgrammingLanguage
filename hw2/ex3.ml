exception EmptyHeap

type heap = EMPTY
          | NODE of rank * value * heap * heap
and rank = int
and value = int

let rank = function EMPTY -> -1
                  | NODE (r, _, _, _) -> r


let shake (x,lh,rh) =
  if (rank lh) >= (rank rh)
  then NODE(rank rh+1, x, lh, rh)
  else NODE(rank lh+1, x, rh, lh)

let merge (h: (heap * heap)):heap = 
  let rec buildStack ((stack, h1, h2): (heap list * heap * heap)):(heap list *
                                                                  heap) =
    (* pile up stack until EMPTY node reached 
     * Return stack and last node processed
     *)
    match (h1, h2) with
    | (EMPTY, _) -> (stack, h2)
    | (_, EMPTY) -> (stack, h1)
    | (NODE (_, v1, _, rh1), NODE (_, v2, _, rh2)) ->
      if v1 < v2 then buildStack (h1::stack, rh1, h2)
      else buildStack (h2::stack, h1, rh2)
  in

  let ((stack, lastNode): (heap list * heap)) = 
    buildStack ([], fst h, snd h) 
  in
  let rec heapify((s, lN): (heap list * heap)):heap = 
    (* s: stack
     * lN: lastNode processed *)
    match s with 
    | EMPTY :: _ -> raise EmptyHeap
    | [] -> lN
    | NODE (_, v, lr, _) :: tl -> heapify(tl, shake (v, lr, lN))
  in
  heapify (stack, lastNode)

let findMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,lh,rh) -> merge (lh,rh)

