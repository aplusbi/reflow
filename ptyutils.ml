type winsize = { ws_row:int; ws_col:int; ws_xpixel:int; ws_ypixel:int }

external forkpty: unit -> (int * Unix.file_descr * string) = "ocaml_forkpty"
external openpty: unit -> (int * Unix.file_descr * Unix.file_descr * string) = "ocaml_openpty"
external get_winsize: Unix.file_descr -> winsize = "ocaml_get_winsize"
external set_winsize: Unix.file_descr -> winsize -> unit = "ocaml_set_winsize"
external sigwinch_fun: unit -> int = "ocaml_sigwinch"

let sigwinch = sigwinch_fun ();;

(* Curses utilities*)
let process_rb rb =
  let attrs = ref 0 in
  let fg = ref 0 in
  let bg = ref 0 in
  let esc = ref false in
  let process i =
    let c = Ringbuffer.get rb i in
    match !esc with
      | true -> begin
          match c with
            | 'm' -> Curses.attrset !attrs
            | x when (int_of_char x) = 1 -> attrs := !attrs land Curses.A.bold
            | x when (int_of_char x) >= 30 && (int_of_char x) <= 37 -> ()
            | x when (int_of_char x) >= 40 && (int_of_char x) <= 47 -> ()
            | _ -> ()
        end
      | false -> begin
          match int_of_char c with
            | 0o33 -> esc := true; ()
            | i -> let _ = Curses.addch i in ()
        end
  in
    for i = 0 to Ringbuffer.length rb do
      process 0
    done
