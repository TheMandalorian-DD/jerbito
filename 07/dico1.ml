type 'a t = (int * 'a) list

let empty = []

let find (k: int) (dico: 'a t) : 'a =
    List.assoc k dico

let add (e: int * 'a) (dico: 'a t) : 'a t =
  List.append [e] dico
  (*match dico with
  | [] -> [(c, v)]
  | (c', _)::q when c' = c -> dico
  | t::q -> t::add (c, v) q*)

let del (k: int) (dico: 'a t) : 'a t =
  List.remove_assoc k dico
  (*match dico with
  | [] -> []
  | (c, _)::q when c = k -> dico
  | t::q -> t::del k q*)

