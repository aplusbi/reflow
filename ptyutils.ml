type winsize = { ws_row:int; ws_col:int; ws_xpixel:int; ws_ypixel:int }

external get_winsize: Unix.file_descr -> winsize = "ocaml_get_winsize"
external set_winsize: Unix.file_descr -> winsize -> unit = "ocaml_set_winsize"
external sigwinch_fun: unit -> int = "ocaml_sigwinch"

let sigwinch = sigwinch_fun ();;

let forkpty _ =
        let master = ExtUnix.All.posix_openpt [Unix.O_RDWR] in
        ExtUnix.All.grantpt master;
        ExtUnix.All.unlockpt master;
        let name = ExtUnix.All.ptsname master in
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
