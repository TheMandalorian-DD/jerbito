type 'a t = 'a * 'a list

let empty = []

let unit x = (x, empty)
let cons x (y, u) = x, y :: u
let hd (x, _) = x
let tl (_, u) = match u with
  | [] -> failwith "tl"
  | x :: u -> (x, u)

(*let length ((_, u): 'a t) : int = 
  List.length u
  (*let rec aux (cpt: int) (l: 'a list) = 
    match l with
    | [] -> cpt
    | t::q -> aux (cpt + 1) q
  in aux 0 u*)

(*let append ((a, u): 'a t) ((b, v): 'a t) : 'a t = (a+b, u@v)*)

let concat ((a, u): 'a t t) : 'a t = a

(*let rev ((_, u): 'a t) = List.rev u*)

let map (f: 'a -> 'b) ((a, u): 'a t) : 'b t = 
  (f a, List.map f u)*)


