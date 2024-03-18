(*Question 1*)
let print_list lst =
  List.iter (fun x -> print_int (x)) lst

let last (l: 'a list) : 'a =
  let ll = List.rev l in
  match ll with
  | [] -> failwith "empty list"
  | t::_ -> t

(*Question 2*)
let swap (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | t1::q1 -> match q1 with
           | [] -> [t1]
           | t2::q2 -> t2::t1::q2

(*Question 3*)
let repeat (elem: 'a) (n: int) : 'a list =
  if n = 0 then [] else List.init n (fun _ -> elem)

(*Question 4*)

let rec range_i (i: int) (j: int) : int list =
  if i > j then [] else i::(range_i (i+1) j)
  
(*Question 5*)
let rec decr_list (l: int list) : int list =
  match l with
  | [] -> []
  | t::q -> t-1::decr_list q
  
(*Question 6*)
let rec rev (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | t::q -> rev (q) @ [t]

let rec mem (x: 'a) (l: 'a list) : bool =
  match l with
  | [] -> false
  | t::q -> if t = x then true else mem x q

let append (l1: 'a list) (l2: 'a list) : 'a list =
  l1 @ l2

let rec repeat_rec (elem: 'a) (n: int) : 'a list =
  if n = 0 then [] else elem::(repeat_rec elem (n-1))

(*Question 7*)
let rec flat (ll: 'a list list) : 'a list =
  match ll with
  | [] -> []
  | t::q -> t @ (flat q)

(*Question 8*)
let rec interpose (z: 'a) (ll: 'a list) : 'a list =
  match ll with
  | [] -> []
  | t::[] -> [t]
  | t::q -> t::z::interpose z q

(*Question 9*)
let rec slutter (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | t::q -> t::t::slutter q

(*Question 10*)
let rec add_list (l1: int list) (l2: int list) : int list =
  match l1, l2 with
  | [], _ -> l2
  | _, [] -> l1
  | t1::q1, t2::q2 -> t1+t2::add_list q1 q2

(*Question 11*)
let rec remove_dup (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | t::q -> if mem t q then remove_dup q else t::remove_dup q

(*Question 12*)
let rec is_sorted (l: int list) : bool =
  match l with
  | [] -> true
  | t1::q1 -> match q1 with
           | [] -> true
           | t2::_ -> if t1 <= t2 then is_sorted q1 else false

(*Question 13*)
let incr_list (l: int list) : int list =
  List.map (fun x -> x+1) l

(*Question 14*)
let only_less (n: int) (ll: int list) : int list =
  List.filter (fun x -> x < n) ll

(*Question 15*)
let rev_it (l: 'a list) : 'a list =
  let rec rev_it_rec (acc: 'a list) = function
    | [] -> acc
    | t::q -> rev_it_rec (t::acc) q
  in rev_it_rec [] l

let append_it (l1: 'a list) (l2: 'a list) : 'a list =
  let rec append_iter acc = function
    | [] -> acc
    | h::t -> append_iter (acc @ [h]) t
  in
  append_iter l1 l2

(*Question 16*)
let max (l: int list) : int =
  let rec max_iter (max: int) = function
    | [] -> max
    | t::q -> if t > max then max_iter t q else max_iter max q
  in if l = [] then failwith "Liste vide" else max_iter 0 l

(*Question 17*)
let is_sorted_it (l: int list) : bool =
  let rec is_sorted_it_rec (acc: int) = function
    | [] -> true
    | t::q -> if t <= acc then is_sorted_it_rec t q else false
  in is_sorted_it_rec 0 l

(*Question 18*)
let remove_dup_it (l: 'a list) : 'a list =
  let rec remove_dup_it_rec (acc: 'a list) = function
    | [] -> acc
    | t::q -> if mem t acc then remove_dup_it_rec acc q else remove_dup_it_rec (t::acc) q
  in remove_dup_it_rec [] l