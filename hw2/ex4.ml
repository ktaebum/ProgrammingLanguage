module type Queue = sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

module IntListQ = 
  struct
    type element = int list
    type queue = (element list * element list)
    exception EMPTY_Q
    let emptyQ:queue = ([], [])
    let enQ ((q, elt): (queue * element)):queue = 
      let (lq, rq) = q in
      (elt::lq, rq)
    let deQ (q:queue):(element * queue) = 
      let (lq, rq) = q in
      match (lq, rq) with
      | ([], []) -> raise EMPTY_Q
      | (_, []) -> 
        let reversed = List.rev lq in
        (List.hd reversed, ([], List.tl reversed))
      | (_, _) ->
        (List.hd rq, (lq, List.tl rq))
  end
