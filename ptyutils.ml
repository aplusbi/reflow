type winsize = { ws_row:int; ws_col:int; ws_xpixel:int; ws_ypixel:int }
type termios = { c_iflag:int; c_oflag:int; c_cflag:int; c_lflag:int; c_cc:int array }

external forkpty: (unit -> unit) -> termios option -> winsize option -> (int * Unix.file_descr * string) = "ocaml_forkpty"
external forkpty_nocallback: termios option -> winsize option -> (int * Unix.file_descr * string) = "ocaml_forkpty_nocallback"

external set_winch_handler: (int -> unit) -> unit = "ocaml_handle_winch"
external set_winch_handler_off: unit -> unit = "ocaml_handle_winch"