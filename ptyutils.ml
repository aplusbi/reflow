type winsize = { ws_row:int; ws_col:int; ws_xpixel:int; ws_ypixel:int }

external forkpty: unit -> (int * Unix.file_descr * string) = "ocaml_forkpty"
external openpty: unit -> (int * Unix.file_descr * Unix.file_descr * string) = "ocaml_openpty"
external get_winsize: Unix.file_descr -> winsize = "ocaml_get_winsize"
external set_winsize: Unix.file_descr -> winsize -> unit = "ocaml_set_winsize"
external sigwinch_fun: unit -> int = "ocaml_sigwinch"

let sigwinch = sigwinch_fun ();;

(* Curses utilities*)
let process_buffer get length buff =
  let attrs = ref 0 in
  let fg = ref 0 in
  let bg = ref 0 in
  let esc = ref false in
  let process i =
    let c = get buff i in
    let x = int_of_char c in
    match !esc with
      | true -> begin
          match c with
            | 'm' -> Curses.attrset !attrs; esc := false
            | _ when x = 1 -> attrs := !attrs land Curses.A.bold
            | _ when x >= 30 && x <= 37 -> fg := x - 30
            | _ when x >= 40 && x <= 47 -> bg := x - 40
            | _ -> ()
        end
      | false -> begin
          match int_of_char c with
            | 0o33 -> esc := true
            | i -> let _ = Curses.addch i in ()
        end
  in
    for i = 0 to (length buff)-1 do
      process i
    done
