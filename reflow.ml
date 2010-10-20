module Integer =
struct
  type t = int
  let compare = compare
end
module SigMap = Map.Make(Integer)

let buffsize = 65536
let in_buffsize = 1024
let out_buffsize = 65536
let buffer = Ringbuffer.create buffsize
let in_buffer = String.create in_buffsize
let out_buffer = String.create out_buffsize
let need_reprint = ref false

let child _ =
  if (Array.length Sys.argv) < 2 then
    Unix.execv "/bin/bash" [|"/bin/bash"|]
  else 
    let exec = Array.append [|"/bin/sh"; "-c"|] (Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1)) in
      Unix.execv exec.(0) exec

let sigwinch_handler pid sg  = Unix.kill pid sg; need_reprint := true

let ctrl k = char_of_int ((int_of_char k) land 0x1F)

let signal_map = List.fold_left2 (fun a b c -> SigMap.add b c a) SigMap.empty [Sys.sigint; Sys.sigtstp] [ctrl 'C'; ctrl 'Z']

let sigchar_handler fd sg =
  try
    let c = SigMap.find sg signal_map in
    let _ = Unix.write fd (String.make 1 c) 0 1 in ()
  with Not_found -> ()

let setup_terminal pid fd =
  Sys.set_signal Ptyutils.sigwinch (Sys.Signal_handle (sigwinch_handler pid));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (sigchar_handler fd));
  Sys.set_signal Sys.sigtstp (Sys.Signal_handle (sigchar_handler fd));
  let ws = Ptyutils.get_winsize Unix.stdout in Ptyutils.set_winsize fd ws;
  let close_fd _ = Unix.close fd in at_exit close_fd

let setup _ =
  let _ = Curses.initscr () in at_exit Curses.endwin;
  let _ = Curses.raw () in
  let _ = Curses.noecho () in
  Sys.set_signal Sys.sigchld (Sys.Signal_handle exit)

let _ =
  setup ();
  match Ptyutils.forkpty () with 
    | (-1, _, _) -> failwith "Error"
    | (0, _, _) -> child ()
    | (pid, fd, _) ->
        setup_terminal pid fd;
        let read_len = ref 0 in
        let input_len = ref 0 in
        while true do
          let (input, output, _) = Unix.select [Unix.stdin; fd] [Unix.stdout; fd] [] (-1.) in
            if List.mem fd input then
              begin
              let rd_l = (Unix.read fd out_buffer !read_len (out_buffsize - !read_len)) in
                Ringbuffer.read_from_string out_buffer buffer !read_len rd_l;
                read_len := !read_len + rd_l
              end;
            (if (List.mem Unix.stdout output) then
              begin
                if !need_reprint then
                  let (w, h) = Curses.get_size () in let ws = {(Ptyutils.get_winsize fd) with Ptyutils.ws_row=h; Ptyutils.ws_col=w} in Ptyutils.set_winsize fd ws;
                  (*Ringbuffer.iter (fun x -> Curses.addch (int_of_char x)) buffer; read_len := 0; need_reprint := false*)
                  Ptyutils.process_buffer Ringbuffer.get Ringbuffer.used_length buffer; read_len := 0; need_reprint := false
                  else
                    Ptyutils.process_buffer String.get (fun x -> !read_len) out_buffer; read_len := 0
              end);
            (if List.mem Unix.stdin input then
              begin
                let c = char_of_int (Curses.getch ()) in in_buffer.[!input_len] <- c; input_len := !input_len + 1
              end);
            (if List.mem fd output then
              begin
                let _ = Unix.write fd in_buffer 0 !input_len in input_len := 0
              end);
            let _ = Curses.refresh () in ();
        done
