type t 
val create : int -> t
val unix_read : Unix.file_descr -> t -> int
val unix_write : Unix.file_descr -> t -> int
val read_from_string : string -> t -> int -> int -> unit
val string_of_ringbuffer : t -> string
val substring_of_ringbuffer : t -> int -> int -> string
val iter : (char -> 'a) -> t -> unit
val get : t -> int -> char
val length : t -> int
val used_length : t -> int
