type team = Korea 
          | France
          | Usa
          | Brazil
          | Japan
          | Nigeria
          | Cameroon
          | Poland
          | Portugal
          | Italy
          | Germany
          | Norway
          | Sweden
          | England
          | Argentina;;

let team2string (t:team):string =
  (* Convert given team type to string *)
  match t with
  | Korea -> "Korea"
  | France -> "France" 
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina";;

type tourna = LEAF of team
            | NODE of tourna * tourna;;

let parenize (input: tourna): string =
  let (result:string) = "" in
  let rec traverse (root: tourna):string =
  match root with
  | LEAF t -> result ^ (team2string t) 
  | NODE (n, m) -> 
    (* when entering left child, append (
     * after traverse right child, append ) *)
    result ^ "(" ^ (traverse n) ^ " " ^ (traverse m) ^ ")" in

  traverse input;;


(* Here comes test codes *)
let string2team (s:string):team =
  match s with
  | "Korea" -> Korea
  | "France" -> France
  | "Usa" -> Usa
  | "Brazil" -> Brazil
  | "Japan" -> Japan
  | "Nigeria" -> Nigeria
  | "Cameroon" -> Cameroon
  | "Poland" -> Poland
  | "Portugal" -> Portugal
  | "Italy" -> Italy
  | "Germany" -> Germany
  | "Norway" -> Norway
  | "Sweden" -> Sweden
  | "England" -> England
  | "Argentina" -> Argentina;;

let a = [|"Korea"; "France"; "Usa"; "Brazil"; "Japan"; "Nigeria";
                 "Cameroon"; "Poland"; "Portugal"; "Italy"; "Germany"; "Norway";
                 "Sweden"; "England"; "Argentina"|];;
