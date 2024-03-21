module IntSet = Set.Make(struct
  type t = int
  let compare a b = a - b
end);;

let has_dupes l = 
  (List.length l) <> (IntSet.cardinal(IntSet.of_list l))

module CharMap = Map.Make(Char)



let char_digit (x: int) : char = 
 char_of_int (x+48) 

let 

 

 

