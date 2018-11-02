type require = id * (cond list)
and cond 
  = Items of gift list
  | Same of id (* other nephew's gift *)
  | Common of cond * cond (* Common gift among two condition *)
  | Except of cond * gift list (* except gift in certain condition *)
and gift = int
and id = A | B | C | D | E


module Gift =
  struct
    type t = gift
    let compare g1 g2 = 
      Pervasives.compare g1 g2
  end

(* Define gift set *)
module GiftSet = Set.Make(Gift)

let id2String i =
  match i with
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"


let shoppingList (requires: require list): (id * gift list) list =
  let trimmedRequires = Hashtbl.create 5 in
  let rec trimRequires requires =
    match requires with
    | [] -> ()
    | hd :: tl ->
      let id = fst hd in
      let conds = snd hd in
      let currentConds =
      try
        Hashtbl.find trimmedRequires id
      with Not_found -> []
      in
      if currentConds = [] then
        (* not listed conditions *)
        Hashtbl.add trimmedRequires id conds
      else
        (* merge conditions *)
        Hashtbl.replace trimmedRequires id (conds @ currentConds);
      trimRequires tl
  in
  trimRequires requires;


  let requires = Hashtbl.fold (fun key value rlist -> (key, value) :: rlist) trimmedRequires [] in

  let effectTable = Hashtbl.create 16 in

  let rec parseRequires (requirements: require list) =
    let parseRequire (requirement: require) =

      (* get currentID and corresponding conditions *)
      let currentID = fst requirement in
      let conditions = snd requirement in

      (* define bind conditions procedure *)
      let rec bindConditions (conditions: cond list) =
        let rec bindCondition (condition: cond) =
          match condition with
          | Same otherID ->
            let isTableHas = Hashtbl.find_all effectTable otherID in
            let findCurrentID requireItem = ((fst requireItem) = currentID) in
            let safeFind = 
              try
                List.find findCurrentID isTableHas
              with Not_found -> (currentID, [])
            in
            if ((fst safeFind) = currentID) then Hashtbl.add effectTable otherID requirement
          | Common (condition1, condition2) ->
            bindCondition condition1;
            bindCondition condition2;
          | Except (condition, _) ->
            bindCondition condition
          | Items _ -> ()
        in
        match conditions with
        | [] -> ()
        | hd :: tl ->
          bindCondition hd;
          bindConditions tl
      in
      bindConditions conditions
    in
  match requirements with
  | [] -> ()
  | hd :: tl ->
    parseRequire hd;
    parseRequires tl
  in
 
  parseRequires requires;

  let aGift = ref GiftSet.empty in
  let bGift = ref GiftSet.empty in
  let cGift = ref GiftSet.empty in
  let dGift = ref GiftSet.empty in
  let eGift = ref GiftSet.empty in

  let getID2GiftSet targetID = 
    match targetID with
    | A -> aGift
    | B -> bGift
    | C -> cGift
    | D -> dGift
    | E -> eGift
  in
  
  let rec queryAllConditions conditions =
    let rec queryCondition condition = 
      let rec addItem2Set currentSet items =
        match items with
        | [] -> currentSet
        | hd :: tl ->
          addItem2Set (GiftSet.add hd currentSet) tl
      in

      let rec removeItemInSet currentSet items =
        match items with
        | [] -> currentSet
        | hd :: tl ->
          removeItemInSet (GiftSet.remove hd currentSet) tl
      in

      match condition with
      | Items items ->
        addItem2Set GiftSet.empty items
      | Same otherID ->
        !(getID2GiftSet otherID)
      | Common (condition1, condition2) ->
        GiftSet.inter (queryCondition condition1) (queryCondition condition2)
      | Except (condition, items) ->
        removeItemInSet (queryCondition condition) items
    in
    match conditions with
    | [] -> GiftSet.empty
    | hd :: tl ->
      GiftSet.union (queryCondition hd) (queryAllConditions tl)
  in

  let jobStack = ref [] in

  (* Now calculate while stack has item *)
  let rec pushRequirements requirements =
    match requirements with
    | [] -> ()
    | hd :: tl ->
      jobStack := hd :: !jobStack;
      pushRequirements tl
  in

  let calculate requirement =
    let currentID = fst requirement in
    let conditions = snd requirement in
    let giftSetRef = getID2GiftSet currentID in
    let previousGiftSet = !giftSetRef in
    let affectList = Hashtbl.find_all effectTable currentID in
    giftSetRef := queryAllConditions conditions;
    if not (GiftSet.equal !giftSetRef previousGiftSet) then
      pushRequirements affectList
  in

  pushRequirements requires;
  while (List.length !(jobStack)) > 0 do
    let requirement = List.hd !jobStack in
    jobStack := List.tl !jobStack;
    calculate requirement;
  done;
  [(A, (GiftSet.elements !aGift)); (B, (GiftSet.elements !bGift)); (C, (GiftSet.elements !cGift)); (D, (GiftSet.elements !dGift)); (E, (GiftSet.elements !eGift))]
