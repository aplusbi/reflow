type winsize = { ws_row : int; ws_col : int; ws_xpixel : int; ws_ypixel : int; }
val get_winsize : Unix.file_descr -> winsize
val set_winsize : Unix.file_descr -> winsize -> unit
val sigwinch : int
val posix_openpt : Unix.open_flag list -> Unix.file_descr
val grantpt : Unix.file_descr -> unit
val unlockpt : Unix.file_descr -> unit
val posix_forkpty : unit -> int * Unix.file_descr * string
