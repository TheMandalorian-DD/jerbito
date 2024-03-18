let char_succ (c: char) : char = 
  if c = '\255' then '\000' 
  else char_of_int ((int_of_char c) + 1)

let string_cons (c: char) (mot: string) : string = 
  String.make 1 c ^ mot

let rec char_range (c1: char) (c2: char) : string = 
  if c1 = c2 then string_cons c1 ""
  else string_cons c1 (char_range (char_succ c1) c2)

let char_range_rev (c1: char) (c2: char) : string = 
  let rec range (c1: char) (c2: char) (mot: string) : string =
    if c1 = c2 then (string_cons c1 mot)
    else range (char_succ c1) c2 (string_cons c1 mot)
  in range c1 c2 ""

let rec is_palindrome s =
  let len = String.length s in
  if len < 2 then true
  else if s.[0] <> s.[len-1] then false
  else is_palindrome (String.sub s 1 (len-2))



  

