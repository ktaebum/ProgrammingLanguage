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


(*

let idtest (h) = 
   match h with 
   (id, gift) -> 
   match id with 
   | A -> "A" 
   | B -> "B" 
   | C -> "C" 
   | D -> "D" 
   | E -> "E" 

let gifttest (h) = 
   match h with 
   | (id, gift) -> gift 

let rec printlisttest (gift) = 
   match gift with 
   | [] -> "]" 
   | h::t -> (string_of_int h) ^ printlisttest(t) 

let rec printfinaltest (result) = ( 
   match result with 
   | [] -> () 
   | h::t -> print_string ("(" ^ idtest(h) ^ ",[" ^ printlisttest(gifttest(h)) ^");") 

) 

let check1 = [(A, Items[1;2]::[])] 
let check2 = [(A, Items[1;2]::Items[3;4]::[])] 
let check3 = [(A,Items[1;2]::Items[1;4]::[]);(C,Items[3;4]::[])] 
let check4 = [(A,Items[1;2]::Items[1;4]::[]);(B, Same A::[]);(C,Items[3;4]::[])] 
let check5 = [(A,Items[1;2]::Items[1;4]::[]);(B, Same C::[]);(C,Items[3;4]::[])] 
let check6 = [(A,Same B::[]);(B, Same C::[]);(C, Same D::[]);(D, Same E::[]);(E, Same A::[])] 
let check7 = [(A, Items [1;2;3]::[]); (B,Same A::Items [4]::[]); (C, []); (D, []); (E, [Same D])] 

let check8 = [(A, [Items[1;2]]); (B, [Same A])] 
let check9 = [(A, [Items[1;2]]); (B, [Same C]);(C, [Items[2;3]])] 
let check10 = [(A, [Items[1;2]]); (B, [Items[4];Same C]);(C, [Items[2;3]])] 
let check11 = [(A, [Items[1;2]]); (B, [Same C;Items[4]]);(C, [Items[2;3]])] 
let check12 = [(A, [Items[1;2]; Same C]); (B, [Same A]);(C, [Items[2;3]])] 
let check13 = [(A, [Except (Items[1;2;3], [1;2])])] 
let check14 = [(A, [Items[1;2;3]]);(B, [Except (Same A, [1])])] 
let check15 = [(A, [Common (Items[1;2], Items[2;3])])] 
let check16 = [(A, [Items [5;6]; Common(Items [1;2], Items [2;3])])] 

let check17 = [(A, [Items[1;2;3;4;5]]);(B,[Same D]); (C,[Common (Common (Except (Items [1;2;3;4;5], [3;4]), Same B), Same A)]);(D, [Items [1;3;5;7;9]])] 

let _ = 
if (shoppingList (check1) = [(A,[1;2]);(B,[]);(C,[]);(D,[]);(E,[])])          then print_endline("1") else printfinaltest(shoppingList(check1)); 
if (shoppingList (check2) = [(A,[1;2;3;4]);(B,[]);(C,[]);(D,[]);(E,[])])       then print_endline("2") else printfinaltest(shoppingList(check2)); 
if (shoppingList (check3) = [(A,[1;2;4]);(B,[]);(C,[3;4]);(D,[]);(E,[])])       then print_endline("3") else printfinaltest(shoppingList(check3)); 
if (shoppingList (check4) = [(A,[1;2;4]);(B,[1;2;4]);(C,[3;4]);(D,[]);(E,[])])    then print_endline("4") else printfinaltest(shoppingList(check4)); 
if (shoppingList (check5) = [(A,[1;2;4]);(B,[3;4]);(C,[3;4]);(D,[]);(E,[])])    then print_endline("5") else printfinaltest(shoppingList(check5)); 
if (shoppingList (check6) = [(A,[]);(B,[]);(C,[]);(D,[]);(E,[])])             then print_endline("6") else printfinaltest(shoppingList(check6)); 
if (shoppingList (check7) = [(A,[1;2;3]);(B,[1;2;3;4]);(C,[]);(D,[]);(E,[])])   then print_endline("7") else printfinaltest(shoppingList(check7)); 
if (shoppingList (check8) = [(A,[1;2]);(B,[1;2]);(C,[]);(D,[]);(E,[])])       then print_endline("8") else printfinaltest(shoppingList(check8)); 
if (shoppingList (check9) = [(A,[1;2]);(B,[2;3]);(C,[2;3]);(D,[]);(E,[])])       then print_endline("9") else printfinaltest(shoppingList(check9)); 
if (shoppingList (check10) = [(A,[1;2]);(B,[2;3;4]);(C,[2;3]);(D,[]);(E,[])])    then print_endline("10") else printfinaltest(shoppingList(check10)); 
if (shoppingList (check11) = [(A,[1;2]);(B,[2;3;4]);(C,[2;3]);(D,[]);(E,[])])    then print_endline("11") else printfinaltest(shoppingList(check11)); 
if (shoppingList (check12) = [(A,[1;2;3]);(B,[1;2;3]);(C,[2;3]);(D,[]);(E,[])]) then print_endline("12") else printfinaltest(shoppingList(check12)); 
if (shoppingList (check13) = [(A,[3]);(B,[]);(C,[]);(D,[]);(E,[])])          then print_endline("13") else printfinaltest(shoppingList(check13)); 
if (shoppingList (check14) = [(A,[1;2;3]);(B,[2;3]);(C,[]);(D,[]);(E,[])])       then print_endline("14") else printfinaltest(shoppingList(check14)); 
if (shoppingList (check15) = [(A,[2]);(B,[]);(C,[]);(D,[]);(E,[])])          then print_endline("15") else printfinaltest(shoppingList(check15)); 
if (shoppingList (check16) = [(A,[2;5;6]);(B,[]);(C,[]);(D,[]);(E,[])])       then print_endline("16") else printfinaltest(shoppingList(check16)); 
if (shoppingList (check17) = [(A,[1;2;3;4;5]);(B,[1;3;5;7;9]);(C,[1;5]);(D,[1;3;5;7;9]);(E,[])]) then print_endline("17") else printfinaltest(shoppingList(check17)); 

(*********************************************************************************) 

let emptyL = [(A, []); (B, []); (C, []); (D, []); (E, [])] in 
assert ((shoppingList []) = emptyL); 
print_endline "a"; 

assert ((shoppingList [ 
(A, []); (B, []); (C, []); (D, []); (E, []); 
]) = emptyL); 
print_endline "b"; 

assert ((shoppingList [ 
(A, [Same B]); (B, [Same C]); (C, [Same D]); (D, [Same E]); (E, [Same A]); 
]) = emptyL); 
print_endline "c"; 

assert ((shoppingList [ 
(A, [Items [1;2;3]]); (B, [Items [2;3;4]]); 
(C, [Items [3;4;1]]); (D, [Items [4;1;2]]); 
(E, [Items [1;2;3;1;2;3]]); 
]) = [(A, [1; 2; 3]); (B, [2; 3; 4]); (C, [1; 3; 4]); (D, [1; 2; 4]); (E, [1; 2; 3])]); 
print_endline "d"; 

assert ((shoppingList [ 
(A, [Items [1;2;3]]); 
(B, [Same A]); 
(C, [Same A; Items[1;2]]); 
(D, [Same A; Items[4]]); 
(E, [Same D]); 
]) = [(A, [1; 2; 3]); (B, [1; 2; 3]); (C, [1; 2; 3]); (D, [1; 2; 3; 4]); (E, [1; 2; 3; 4])]); 
print_endline "e"; 

assert ((shoppingList [ 
(A, [Common (Items [1;2;3], Items [2;1;3])]); 
(B, [Common (Items [2;1;3], Items [5;6;1;4;2;3])]); 
(C, [Common (Items [1;2;3], Items [4;5;6])]); 
(D, [Common (Items [3;2;1], Items [1])]); 
(E, [Common (Items [1;2;3], Items [])]); 
]) = [(A, [1; 2; 3]); (B, [1; 2; 3]); (C, []); (D, [1]); (E, [])]); 
print_endline "f"; 

assert ((shoppingList [ 
(B, [Common (Items [2;1;3], Items [5;6;1;4;2;3])]); 
(E, [Common (Items [], Items [])]); 
(D, [Common (Items [1], Items [1])]); 
]) = [(A, []); (B, [1; 2; 3]); (C, []); (D, [1]); (E, [])]); 
print_endline "g"; 

assert ((shoppingList [ 
(A, [Except (Items [3;2;1], [3;2;1])]); 
(B, [Except (Items [2;1;3], [])]); 
(C, [Except (Items [2;1;3], [1;2;3;4;5;6])]); 
(D, [Except (Items [], [2;1;3])]); 
(E, [Except (Items [], [])]); 
]) = [(A, []); (B, [1; 2; 3]); (C, []); (D, []); (E, [])]); 
print_endline "h"; 

assert ((shoppingList [ 
(A, [Common (Common (Same B, Same C), Common (Same D, Same E))]); 
(B, [Common (Same C, Common (Same D, Except (Same E, [5])))]); 
(C, [Same D; Items[7;8]]); 
(D, [Except (Same E, [1;2;3])]); 
(E, [Items [1;2;3;4;5]]); 
]) = [(A, [4]); (B, [4]); (C, [4; 5; 7; 8]); (D, [4; 5]); (E, [1; 2; 3; 4; 5])]); 
print_endline "i"; 

assert ((shoppingList [ 
(A, [Same B; Same C]); 
(B, [Except (Same C, [1;2;3]); Same D]); 
(C, [Items [1;2;3]; Items [3;4;5]; Common (Same A, Items [6;7])]); 
(D, [Same E]); 
(E, [Same D; Items[6;8]]); 
]) = [(A, [1; 2; 3; 4; 5; 6; 8]); (B, [4; 5; 6; 8]); (C, [1; 2; 3; 4; 5; 6]); (D, [6; 8]); (E, [6; 8])]); 
print_endline "j"; 

assert ((shoppingList [ 
(A, [Common (Same B, Common (Except (Items [1;2;3;4;5], [1;3;5]), Same C)); Items [2;4;8]]); 
(B, [Except (Except (Except (Same A, [1]),[1;2]),[3]); Items [3;6;9]]); 
(C, [Same A; Same B; Same D; Same E]); 
(D, [Items [10]; Common (Same A, Same D); Items [5]]); 
(E, [Common (Same C, Common (Same A, Common (Same D, Same B)))]) 
]) = [(A, [2; 4; 8]); (B, [3; 4; 6; 8; 9]); (C, [2; 3; 4; 5; 6; 8; 9; 10]); (D, [5; 10]); (E, [])]); 
print_endline "k"; 
assert ((shoppingList [ 
(A, [Items [1;2;3;1;2;3]]); 
(D, [Items [5;5;5;5;5]]); 
(A, [Same D]); 
(E, [Except (Items [1;2;3;1;2;3], [1;2;3])]); 
(A, [Items [1;2;3;4]]); 
]) = [(A, [1; 2; 3; 4; 5]); (B, []); (C, []); (D, [5]); (E, [])]); 
print_endline "l";
   *)
