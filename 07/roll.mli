type 'a t

val unit : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val hd : 'a t -> 'a
val tl : 'a t -> 'a t
(*val length : 'a t -> int
(*val append : 'a t -> 'a t -> 'a t*)
val concat : 'a t t -> 'a t
(*val rev : 'a t -> 'a t*)*)
