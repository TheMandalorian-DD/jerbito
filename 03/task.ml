(*Type énumérés*)
(*Question 1*)
type partie = Victoire | Defaite | Egalite

let resultat (p: partie) : float = 
  match p with
  | Victoire -> 1.
  | Defaite -> 0.
  | Egalite -> 0.5

(*Question 2*)
type semaine = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche

let est_un_weekend (s: semaine) : bool =
  match s with
  | Samedi | Dimanche -> true
  | _ -> false

(*Question 3*)
(*Question 4*) 
(*Question 5*)
(*
type etat = Metro | Boulot | Dodo | Vacance
let action_suivante (e: etat) : etat =
  match e with
  | Metro -> Boulot
  | Boulot -> Dodo
  | Dodo -> Vacance
  | Vacance -> Metro*)

(*Question 6*)
type couleur = Pique | Coeur | Carreaux | Trefle 
type valeur = As | Sept | Huit | Neuf | Dix | Valet | Dame | Roi
type carte_a_jouer = Carte of valeur * couleur

let est_rouge (c: carte_a_jouer) : bool =
  match c with
  | Carte (_, Coeur) -> true
  | Carte (_, Carreaux) -> true
  | _ -> false

let est_une_tete (c: carte_a_jouer) : bool =
  match c with
  | Carte (Valet, _) -> true
  | Carte (Dame, _) -> true
  | Carte (Roi, _) -> true
  | _ -> false

let score_normal (c: carte_a_jouer) : int =
  match c with
  | Carte (As, _) -> 11
  | Carte (Dix, _) -> 10
  | Carte (Roi, _) -> 4
  | Carte (Dame, _) -> 3
  | Carte (Valet, _) -> 2
  | _ -> 0

let score_atout (c: carte_a_jouer) : int =
  match c with
  | Carte (Valet, _) -> 20
  | Carte (Neuf, _) -> 14
  | Carte (As, _) -> 11
  | Carte (Dix, _) -> 10
  | Carte (Roi, _) -> 4
  | Carte (Dame, _) -> 3
  | _ -> 0

let premiere_plus_forte (c1: carte_a_jouer) (c2: carte_a_jouer) (c3: carte_a_jouer) (c4: carte_a_jouer) : bool =
  let s1 = score_atout c1 + score_normal c1 in 
  let s2 = score_atout c2 + score_normal c2 in
  let s3 = score_atout c3 + score_normal c3 in
  let s4 = score_atout c4 + score_normal c4 in
  s1 > s2 && s1 > s3 && s1 > s4

(*Constructeurs avec données*)
(*Question 1*)
type number = Int of int | Float of float

(*Question 2*)
let somme (x: number) (y: number) : number = 
  match x, y with
  | Int a, Int b -> Int (a + b)
  | Float a, Float b -> Float (a +. b)
  | Int a, Float b | Float b, Int a -> Float (float_of_int a +. b)

let difference (x: number) (y: number) : number = 
  match x, y with
  | Int a, Int b -> Int (a - b)
  | Float a, Float b -> Float (a -. b)
  | Int a, Float b | Float b, Int a -> Float (float_of_int a -. b)

let multiplication (x: number) (y: number) : number = 
  match x, y with
  | Int a, Int b -> Int (a * b)
  | Float a, Float b -> Float (a *. b)
  | Int a, Float b | Float b, Int a -> Float (float_of_int a *. b)

let division (x: number) (y: number) : number = 
  match x, y with
  | Int a, Int b -> Float (float_of_int a /. float_of_int b)
  | Float a, Float b -> Float (a /. b)
  | Int a, Float b -> Float (float_of_int a /. b)
  | Float a, Int b -> Float (a /. float_of_int b)







(*Question 3*)
type temperature = Celsius of float 

(*Question 4*)
let est_rouge (c: carte_a_jouer) : bool =
  match c with
  | Carte (_, Coeur) | Carte (_, Carreaux) -> true
  | _ -> false

let est_une_tete (c: carte_a_jouer) : bool =
  match c with
  | Carte (Valet, _) | Carte (Dame, _) | Carte (Roi, _) -> true
  | _ -> false 

(*Types enregistrements (ou produit)*)
(*Question 1*)
type point = { x: float; y: float }

(*Question 2*)
type carte_a_jouer_j = {
  valeur: valeur;
  couleur: couleur;
}

let est_rouge (c: carte_a_jouer_j) : bool =
  match c with
  | {valeur = _; couleur = Carreaux | Coeur} -> true
  | _ -> false
  

let est_une_tete (c: carte_a_jouer_j) : bool =
  match c with
  | {valeur = Roi | Dame | Valet; couleur = _} -> true
  | _ -> false

(*Combinaison de types produits et types*)
(*Question 1*)

(*Types option et list*)
(*Question 1*)
type 'a option = 
  | None   (** La valeur est absente *)
  | Some of 'a  (** La valeur est présente *)

let return (a: int) (b: int) : int option =
  if b = 0 then None else Some (a / b)

(*Question 2*)
let salutation (s: string option) : string =
  match s with
  | None -> "Bonjour, quel est ton nom ?" 
  | Some s -> "Bonjour, " ^ s

(*Question 3*)
type 'a list = 
  | []   (** La liste est vide *)
  | ( :: ) of 'a * 'a list  (** Un élément en tête, et le reste de la liste *)

let commence_par_un_trois (l: 'a list) : bool =
  match l with
  | [] -> false
  | (t::_) -> t = 3 
  | _ -> false

(*Question 4*)
let premier_element (l: 'a list) : 'a option =
  match l with
  | [] -> None
  | t::_ -> Some t

type json =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of json list
  | Object of (string * json) list

(*let rec vers_chaine (j : json) : string = 
  match j with
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Array lst -> 
    let str_lst = List.map (fun item -> vers_chaine item) lst in
    "[" ^ String.concat ", " str_lst ^ "]"
  | Object obj_lst -> 
    let str_pairs = List.map (fun (key, value) -> "\"" ^ key ^ "\": " ^ vers_chaine value) obj_lst in
    "{" ^ String.concat ", " str_pairs ^ "}"*)

type 'a btree =
    Empty
  | Leaf of 'a
  | Node of ('a * 'a btree * 'a btree)