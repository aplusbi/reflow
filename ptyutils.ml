type winsize = { ws_row:int; ws_col:int; ws_xpixel:int; ws_ypixel:int }

external forkpty: unit -> int * Unix.file_descr * string = "ocaml_forkpty"
external get_winsize: Unix.file_descr -> winsize = "ocaml_get_winsize"
external set_winsize: Unix.file_descr -> winsize -> unit = "ocaml_set_winsize"
external sigwinch_fun: unit -> int = "ocaml_sigwinch"

let sigwinch = sigwinch_fun ();;

let posix_openpt flags =
        try
                Unix.openfile "/dev/ptmx" flags 0
        with Unix.Unix_error(err, _, arg) -> raise (Unix.Unix_error(err, "posix_openpt", arg))

external grantpt: Unix.file_descr -> unit = "ocaml_grantpt"
external unlockpt: Unix.file_descr -> unit = "ocaml_unlockpt"
external ptsname: Unix.file_descr -> string = "ocaml_ptsname"

let posix_forkpty _ =
        let master = posix_openpt [Unix.O_RDWR] in
        grantpt master;
        unlockpt master;
        let name = ptsname master in
        let slave = Unix.openfile name [Unix.O_RDWR; Unix.O_NOCTTY] 0 in
        match Unix.fork () with
        | 0 ->
                Unix.close master;
                ignore (Unix.setsid ());
                Unix.dup2 slave Unix.stdout;
                Unix.dup2 slave Unix.stdin;
                Unix.dup2 slave Unix.stderr;
                (0, slave, name)
        | pid ->
                Unix.close slave;
                (pid, master, name)
