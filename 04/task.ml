(*Question 1*)
let first ((a, _): 'a * 'a) : 'a =
  a

(*Question 2*)
let sum ((a, b): int * string) ((c, d): int * string) : int * string =
  (a + c, b ^ d)

(*Question 3*)
type 'a option = 
  | None
  | Some of 'a

let suffix_prefixe (str: string) (c: char) : char option * char option =
  let rec aux (i: int) : char option * char option =
    if (i > String.length str - 1) then (None, None) 
    else if str.[i] = c && i = 0 then (None, Some str.[i+1])
    else if str.[i] = c && i = String.length str - 1 then (Some str.[i-1], None)
    else if str.[i] = c then (Some str.[i-1], Some str.[i+1])
    else aux (i+1)
  in aux 0

(*Question 4*)
let rec zip (l1: 'a list) (l2: 'a list) : ('a * 'a) list =
  match l1, l2 with
  | [], [] -> []
  | [], _ | _, [] -> raise (Invalid_argument "Listes de taille différente")
  | t1::q1, t2::q2 -> (t1, t2)::zip q1 q2   

let rec unzip (l: ('a * 'a) list) : ('a list * 'a list) =
  match l with
  | [] -> ([], [])
  | (a, b)::q -> let (l1, l2) = unzip q in (a::l1, b::l2)

(*Question 5*)
exception Empty

let min_max (l: int list) : (int * int) =
  let rec aux (ll: int list) (mn: int) (mx: int) : (int * int) =
    match ll with
    | [] -> (mn, mx)
    | t::q -> let min = (min t mn) in
                let max = (max t mx) in
                  aux q min max
  in if l = [] then raise Empty else aux l (List.hd l) (List.hd l)

let rec min_max_rec (l: int list) : (int * int) =
  match l with
  | [] -> raise Empty
  | [t] -> (t, t)
  | t::q -> let (mn, mx) = min_max_rec q in (min t mn), (max t mx)

let min_max_left (l: int list) : (int * int) =
  match l with
  | [] -> raise Empty
  | t::q -> List.fold_left (fun (a, b) x -> (min x a, max x b)) (t, t) q
  
(*Question 6*)
type subject = Math | English | History | Sport
type grade = int
type report = (subject * grade list) list

exception Subject_not_found
exception No_grade of subject

let add_subject (s: subject) (r: report) : report = 
  let rec aux (rr: report) : report =
    match rr with
    | [] -> (s, [])::rr
    | (s', _)::q when s' = s -> rr
    | t::q -> t::aux q
  in aux r

let add_subject_s (s: subject) (r: report) : report = 
  if List.exists (fun (s', _) -> s' = s) r then
    r
  else
    (s, [])::r

let add_grade (s: subject) (g: grade) (r: report) : report =
  let l = List.map (fun (s', g') -> if s = s' then (s', g::g') else (s', g')) r in
  if List.for_all2 (=) l r then raise Subject_not_found else l

let add_grade_rec (s: subject) (g: grade) (r: report) : report =
  let rec aux = function
    | [] -> raise Subject_not_found
    | (s', g')::q when s' = s -> (s', g::g')::q
    | t::q -> t::aux q
  in aux r

let get_grades (s: subject) (r: report) : grade list = 
  let rec aux = function
    | [] -> raise Subject_not_found
    | (s', g')::_ when s' = s -> g'
    | t::q -> aux q
  in aux r

let get_best_grade (s: subject) (r: report) : grade =
  let rec max_grade (max: grade) = function
    | [] -> max
    | t::q -> if t > max then max_grade t q else max_grade max q
  in max_grade 0 (get_grades s r)