(*(*Files*)
type 'a queue = {
  front : 'a list;
  rear_rev : 'a list
}

let empty = { front = []; rear_rev = [] }

let is_empty q = q.front = []

let hd q = List.hd q.front

let check = function (* private *)
| { front = []; rear_rev } -> { front = List.rev rear_rev; rear_rev = [] }
| q -> q

let snoc x q = check { q with rear_rev = x :: q.rear_rev }

let tl q = check { q with front = List.tl q.front }

let uncons = function
| { front = x :: front; _ } as q -> Some (x, { q with front })
| _ -> None

let rec of_list (q: 'a queue) (l: 'a list) : 'a queue =
  match l with
  | [] -> q
  | t::u -> of_list (snoc t q) u

let of_list (u: 'a list) : 'a queue =
  let rec aux (q: 'a queue) = function 
    | [] -> q
    | t::u -> aux (snoc t q) u
  in aux empty u

let rec to_list (q: 'a queue) : 'a list =
  if is_empty q then [] else hd q :: to_list (tl q)

(*Fct association*)
let def (f:'a -> 'b) (x: 'a) (y: 'b) (z: 'a) = 
  if x <> z then f z else y

let undefined = failwith "undefined"

let sq (x: int) : int = 
  def undefined x (x*x) x






type 'a tstamp = { data : 'a; date : float; }

let tstamp_join (ts: ('a tstamp) tstamp) : 'a tstamp = 
  if ts.data.date > ts.date then ts.data 
  else {data = ts.data.data; date = ts.date}

let tstamp_dupe (ts: 'a tstamp) : ('a tstamp) tstamp = 
  {data = ts; date = ts.date}

let tstamp_map (f: 'a -> 'b) (ts: 'a tstamp) : 'b tstamp = 
  {data = f ts.data; date = ts.date}

(*Observable*)
type 'a obs = 'a tstamp list  

let list_last l = List.hd (List.rev l)
let delay x y = max 0. (y -. x)

let obs_concat (a: 'a obs) (b:'a obs) : 'a obs =
  match a, b with
  | [], [] -> []
  | u, [] -> u
  | [], v -> v
  | u, v -> let l = List.map (fun {data; date} -> {data; date = date +. (delay (list_last u).date (List.hd v).date)}) v 
            in u @ l*)

