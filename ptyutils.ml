type winsize = { ws_row:int; ws_col:int; ws_xpixel:int; ws_ypixel:int }

external forkpty: unit -> (int * Unix.file_descr * string) = "ocaml_forkpty"
external openpty: unit -> (int * Unix.file_descr * Unix.file_descr * string) = "ocaml_openpty"
external get_winsize: Unix.file_descr -> winsize = "ocaml_get_winsize"
external set_winsize: Unix.file_descr -> winsize -> unit = "ocaml_set_winsize"
external sigwinch_fun: unit -> int = "ocaml_sigwinch"

let sigwinch = sigwinch_fun ();;
