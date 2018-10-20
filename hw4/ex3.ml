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


(*
let test1 =
  let requires = [ (A, [Same B])
          ; (B, [Same A])
          ; (C, [Same B])
          ; (D, [])
          ; (E, [])
          ] in
  shoppingList requires

let test2 = 
  let requires = [ (A, [Except (Same B, [1])])
          ; (B, [Except (Same A, [2])])
          ; (C, [Except (Same B, [3])])
          ; (D, [])
          ; (E, [])
          ] in
  shoppingList requires

let test3 =
  let requires = [ (A, [Same B; Same C])
          ; (B, [Same A; Same C])
          ; (C, [Same A; Same B])
          ; (D, [])
          ; (E, [])
          ] in
  shoppingList requires

let test4 = 
  let requires = [ (A, [Items [1]])
          ; (B, [Items [2]])
          ; (C, [Items [3]])
          ; (D, [])
          ; (E, [])
          ] in
  shoppingList requires

let test5 =
  let requires = [ (A, [Items [1;2]; Common (Same B, Same C)])
          ; (B, [Common (Same C, Items [2;3])])
          ; (C, [Items [1]; Except (Same A, [3])])
          ; (D, [])
          ; (E, [])
          ] in
  shoppingList requires

let test6 =
  let requires = [ (A, [])
          ; (B, [])
          ; (C, [])
          ; (D, [])
          ; (E, [])
          ] in
  shoppingList requires

let test7 = 
  let requires = [ (A, [Items [1;2;3]])
          ; (B, [Same A])
          ; (C, [Same A])
          ; (D, [Same A])
          ; (E, [Same A])
          ] in
  shoppingList requires

let test8 = 
  let requires = [ (A, [Items [1;2;3;4]])
          ; (B, [Items [2;3;4;5]])
          ; (C, [Common (Same A, Same B)])
          ; (D, [Items [3;4;5;6]])
          ; (E, [Common (Same C, Same D)])
          ] in
	shoppingList requires


let test9 = 
	let requires = [ (A, [Items [1;2;3;4;5;6;7]])
          ; (B, [Except (Same A, [1;4])])
          ; (C, [Except (Same B, [2;3])])
          ; (D, [Except (Same C, [5;6])])
          ; (E, [Except (Same D, [7])])
          ] in
  shoppingList requires

let test10 = 
  let requires = [ (A, [Items [1; 2; 3]; Except (Items [5; 6; 7], [6]); Common (Same (D), Same (E))])
          ; (B, [Common (Same (A), Same (B)); Common (Same (B), Same (C)); Except (Same (D), [9])])
          ; (C, [Common (Same (B), Same (C)); Except (Same (E), [1; 6]); Common (Same (A), Same (D))])
          ; (D, [Items [4; 5; 6; 7; 8; 9; 10]; Same (B); Same (C)])
          ; (E, [Except (Same (A), [3]); Items [9; 10; 11]; Common (Common (Same (B), Same (D)), Items [1; 2; 3; 4; 5; 6; 7])])
          ] in
  shoppingList requires

let test11 =
  let requires = [ (A, [Items [1;2;3]])
          ; (B, [Items [2;3;4]])
          ; (C, [Items [3;4;1]])
          ; (D, [Items [4;1;2]])
          ; (E, [Items [1;2;3;1;2;3]])
					] in
	shoppingList requires

let test12 = 
let requires = [ (A, [Items [1;2;3]])
          ; (B, [Same A])
          ; (C, [Same A; Items[1;2]])
          ; (D, [Same A; Items[4]])
          ; (E, [Same D])
          ] in
shoppingList requires

let test13 =
let requires = [ (A, [Common (Items [1;2;3], Items [2;1;3])])
          ; (B, [Common (Items [2;1;3], Items [5;6;1;4;2;3])])
          ; (C, [Common (Items [1;2;3], Items [4;5;6])])
          ; (D, [Common (Items [3;2;1], Items [1])])
          ; (E, [Common (Items [1;2;3], Items [])])
          ] in
shoppingList requires

let test14 = 
let requires = [ (B, [Common (Items [2;1;3], Items [5;6;1;4;2;3])])
          ; (E, [Common (Items [], Items [])])
          ; (D, [Common (Items [1], Items [1])])
          ] in
shoppingList requires

let test15 = 
let requires = [ (A, [Except (Items [3;2;1], [3;2;1])])
          ; (B, [Except (Items [2;1;3], [])])
          ; (C, [Except (Items [2;1;3], [1;2;3;4;5;6])])
          ; (D, [Except (Items [], [2;1;3])])
          ; (E, [Except (Items [], [])])
          ] in
shoppingList requires

let test16 =
let requires = [ (A, [Common (Common (Same B, Same C), Common (Same D, Same E))])
          ; (B, [Common (Same C, Common (Same D, Except (Same E, [5])))])
          ; (C, [Same D; Items[7;8]])
          ; (D, [Except (Same E, [1;2;3])])
          ; (E, [Items [1;2;3;4;5]])
          ] in
shoppingList requires

let test17 =
  let requires = [ (A, [Same B; Same C])
            ; (B, [Except (Same C, [1;2;3]); Same D])
            ; (C, [Items [1;2;3]; Items [3;4;5]; Common (Same A, Items [6;7])])
            ; (D, [Same E])
            ; (E, [Same D; Items[6;8]])
            ] in
  shoppingList requires

let test18 = 
let requires = [ (A, [Common (Same B, Common (Except (Items [1;2;3;4;5], [1;3;5]), Same C)); Items [2;4;8]])
          ; (B, [Except (Except (Except (Same A, [1]),[1;2]),[3]); Items [3;6;9]])
          ; (C, [Same A; Same B; Same D; Same E])
          ; (D, [Items [10]; Common (Same A, Same D); Items [5]])
          ; (E, [Common (Same C, Common (Same A, Common (Same D, Same B)))])
          ] in
shoppingList requires


let test19 =
let requires = [ (A, [Items [1;2;3;1;2;3]; Same D; Items [1;2;3;4]])
          ; (D, [Items [5;5;5;5;5]])
          ; (E, [Except (Items [1;2;3;1;2;3], [1;2;3])])
          ] in
shoppingList requires

let test20 =
let requires = [ (A, [Same B])
          ; (B, [Same C])
          ; (C, [Same D])
          ; (D, [Same E])
          ; (E, [Items [3;1;2]])
          ] in
shoppingList requires

let test21 =
let requires = [ (A, [Items [3;1;2]])
          ; (C, [Same B])
          ; (B, [Same A])
          ; (E, [Same D])
          ; (D, [Same C])
          ] in
shoppingList requires

let test22 =
let requires = [ (A, [Same B])
          ; (B, [Same A])
          ; (C, [])
          ; (D, [])
          ; (E, [])
          ] in
shoppingList requires

let test23 = 
let requires = [ (A, [Items [1]; Same B])
          ; (B, [Items [1]; Same C])
          ; (C, [Items [1]; Same A])
          ] in
shoppingList requires

let test24 = 
let requires = [ (A, [Items [1]; Same B])
          ; (B, [Same C])
          ; (C, [Same B])
          ; (D, [Same A; Common (Same B, Same C)])
          ; (E, [Same A; Same D])
          ] in
shoppingList requires

let test25 =
let requires = [ (A, [Items [1]; Except (Same B, [1])])
          ; (B, [Items [2]; Except (Same C, [2])])
          ; (C, [Items [3]; Except (Same D, [3])])
          ; (D, [Items [4]; Except (Same E, [4])])
          ; (E, [Items [5]; Except (Same A, [5])])
          ] in
shoppingList requires


let test26 =
let requires = [ (A, [Items [1; 2]; Common (Same B, Same C)])
          ; (B, [Common (Same C, Items [2;3])])
          ; (C, [Items [1]; Except (Same A, [3])])
          ; (D, [Common (Same A, Same B)])
          ; (E, [Common (Same A, Same C)])
          ] in
shoppingList requires
*)
