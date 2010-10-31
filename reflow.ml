module Integer =
struct
  type t = int
  let compare = compare
end
module SigMap = Map.Make(Integer)

let buffsize = 65536
let in_buffsize = 256
let out_buffsize = 256
let buffer = Ringbuffer.create buffsize
let in_buffer = String.create in_buffsize
let out_buffer = String.create out_buffsize

let clear_scr _ =
  let str_clear = "\027[H\027[2J" in
    Unix.write Unix.stdout str_clear 0 (String.length str_clear)

let resize fd =
  let {Ptyutils.ws_row=rows; Ptyutils.ws_col=cols} as ws = Ptyutils.get_winsize Unix.stdout in
    Ptyutils.set_winsize fd ws;
    let _ = clear_scr () in
      ignore (Ringbuffer.unix_write Unix.stdout buffer)

let resize_list fd =
  let {Ptyutils.ws_row=rows; Ptyutils.ws_col=cols} as ws = Ptyutils.get_winsize Unix.stdout in
    Ptyutils.set_winsize fd ws;
    let _ = clear_scr () in
    Escutils.process (fun x -> ignore (Unix.write Unix.stdout x 0 (String.length x))) buffer rows cols

let sigwinch_handler pid fd sg = Unix.kill pid sg; resize_list fd

let rec restart_on_EINTR f x =
  try f x with
    | Unix.Unix_error(Unix.EINTR, _, _) -> restart_on_EINTR f x

let setup_terminal pid fd =
  Sys.set_signal Ptyutils.sigwinch (Sys.Signal_handle (sigwinch_handler pid fd));
  let ws = Ptyutils.get_winsize Unix.stdout in
    Ptyutils.set_winsize fd ws;
    let close_fd _ = Unix.close fd in at_exit close_fd

let setup _ =
  let _ = clear_scr () in
  let terminfo = Unix.tcgetattr Unix.stdin in
  let new_terminfo = {terminfo with Unix.c_isig = false; Unix.c_icanon = false; Unix.c_vmin = 0; Unix.c_vtime = 0; Unix.c_echo = false } in
  let reset_stdin () = Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH terminfo in at_exit reset_stdin;
                                                                            Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH new_terminfo;
                                                                            Sys.set_signal Sys.sigchld (Sys.Signal_handle exit)

let child _ =
  if (Array.length Sys.argv) < 2 then
    Unix.execv "/bin/bash" [|"/bin/bash"|]
  else 
    let exec = Array.append [|"/bin/sh"; "-c"|] (Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1)) in
      Unix.execv exec.(0) exec

let main pid fd =
        setup_terminal pid fd;
        while true do
          let (input, _, _) = restart_on_EINTR (Unix.select [Unix.stdin; fd] [] []) (-1.) in if List.mem fd input then
              begin
                let len = Unix.read fd out_buffer 0 out_buffsize in
                  Ringbuffer.read_from_string out_buffer buffer 0 len;
                  ignore (Unix.write Unix.stdout out_buffer 0 len) 
              end;
            if List.mem Unix.stdin input then
              let len = (Unix.read Unix.stdin in_buffer 0 in_buffsize) in
                ignore (Unix.write fd in_buffer 0 len)
        done

let _ =
  setup ();
  match Ptyutils.forkpty () with 
    | (-1, _, _) -> failwith "Error"
    | (0, _, _) -> child ()
    | (pid, fd, _) -> Unix.handle_unix_error (main pid) fd
