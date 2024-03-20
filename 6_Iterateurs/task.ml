let rec map (f: 'a -> 'b) (l: 'a list) : 'b list = 
  match l with
  | [] -> []
  | t::q -> (f t)::map f q

let map2 (f: int -> 'a -> 'b ) (l: 'a list) : 'b list =
  let rec mapi i = function
    | [] -> []
    | t::q -> f i t :: mapi (i+1) q
  in mapi 0 l 

let rec fold_left f acc lst = match lst with
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs

let rec fold_right f lst acc = match lst with
  | [] -> acc
  | x :: xs -> f x (fold_right f xs acc)

let rec filter (f: 'a -> bool) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | t::q -> if (f t) then t::filter f q else filter f q

let rec filter_map (f: 'a -> 'b option) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | t::q -> let res = (f t) in
              match res with
              | None -> filter_map f q
              | Some res -> res::filter_map f q

