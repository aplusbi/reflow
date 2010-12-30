type winsize = { ws_row : int; ws_col : int; ws_xpixel : int; ws_ypixel : int; }
val get_winsize : Unix.file_descr -> winsize
val set_winsize : Unix.file_descr -> winsize -> unit
val sigwinch : int
val forkpty : unit -> int * Unix.file_descr * string
