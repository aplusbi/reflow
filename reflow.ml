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
  let ws = Ptyutils.get_winsize Unix.stdout in Ptyutils.set_winsize fd ws;
  let close_fd _ = Unix.close fd in at_exit close_fd

let clear_scr _ =
  let str_clear = "\027[H\027[2J" in
    Unix.write Unix.stdout str_clear 0 (String.length str_clear)


let setup _ =
  let _ = clear_scr () in
  let terminfo = Unix.tcgetattr Unix.stdin in
  let new_terminfo = {terminfo with Unix.c_isig = false; Unix.c_icanon = false; Unix.c_vmin = 0; Unix.c_vtime = 10; Unix.c_echo = false } in
  let reset_stdin () = Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH terminfo in at_exit reset_stdin;
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH new_terminfo;
  Sys.set_signal Sys.sigchld (Sys.Signal_handle exit)

let resize fd =
  let {Ptyutils.ws_row=rows; Ptyutils.ws_col=cols} as ws = Ptyutils.get_winsize Unix.stdout in
    Ptyutils.set_winsize fd ws;
    let _ = clear_scr () in
    let lines = Escutils.process buffer rows cols in
    List.iter (fun x -> ignore (Unix.write Unix.stdout x 0 (String.length x)); ignore (Unix.write Unix.stdout "\n" 0 1)) lines;
    need_reprint := false

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
          let (input, output, _) = Unix.select [Unix.stdin; fd] [fd] [] (5.) in
            if List.mem fd input then
              begin
              match (Unix.read fd out_buffer !read_len (out_buffsize - !read_len)) with
                | 0 -> ()
                | rd_l -> Ringbuffer.read_from_string out_buffer buffer !read_len rd_l;
                read_len := !read_len + rd_l
              end;
            if !read_len != 0 then
              begin
                if !need_reprint then
                  (resize fd; read_len := 0)
                else
                  let _ = Unix.write Unix.stdout out_buffer 0 !read_len in read_len := 0
              end;
            if List.mem Unix.stdin input then
              input_len := !input_len + (Unix.read Unix.stdin in_buffer 0 in_buffsize);
            if List.mem fd output && !input_len != 0 then
              let _ = Unix.write fd in_buffer 0 !input_len in input_len := 0
        done
