(*let hello_world () : unit = print_string ("Hello word")

let print_int_option (x: int option) : unit = 
  match x with
  | None -> raise(Invalid_argument "Null !")
  | Some x -> print_int x

type json =
| Null
| Bool of bool
| Int of int
| Float of float
| String of string
| Array of json list
| Object of (string * json) list

let rec print_json (j: json) = 
  match j with
  | Null -> print_string "Null !"
  | Bool j -> print_string (string_of_bool j)
  | Int j -> print_int j
  | Float j -> print_float j
  | String j -> print_string j
  | Array j -> print_string "[";
               let rec aux = function
                 | [] -> print_string ""
                 | t::[] -> print_json t;
                 | t::q -> print_json t; 
                           print_string ", ";
                           aux q
               in aux j;
               print_string "]" 
  | Object j -> print_string "{";
                List.iter (fun (a, b) -> print_string a; print_string ":"; print_json b; print_string ";") j;
                print_string "}"


let fibonacci (n: int) : int =
  let array = Array.make (n+1) 0 in
  array.(0) <- 0;
  array.(1) <- 1;
  let rec fibo (n: int) : int =
    if n <= 1 || array.(n) <> 0 then array.(n)
    else 
      let r = fibo (n-1) + fibo (n-2) in
      array.(n) <- r;
      array.(n)
  in 
  fibo n*)
    





