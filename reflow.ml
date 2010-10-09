let quit_message _ =
  let msg = "\nQuitting...\n" in
  let _ = Unix.write Unix.stdout msg 0 (String.length msg) in ()

let child _ = Unix.execv "/bin/bash" [|"/bin/bash"|]

let buffsize = 65536
let in_buffsize = 1024
let out_buffsize = 65536
let buffer = Ringbuffer.create buffsize
let in_buffer = String.create in_buffsize
let out_buffer = String.create out_buffsize
let need_reprint = ref false

let winch pid sg  = Unix.kill pid sg; need_reprint := true

let setup_signal_passing pid =
  Sys.set_signal Sys.sighup (Sys.Signal_handle (Unix.kill pid));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (Unix.kill pid));
  Sys.set_signal Sys.sigcont (Sys.Signal_handle (Unix.kill pid));
  Sys.set_signal Sys.sigtstp (Sys.Signal_handle (Unix.kill pid))

let ctrl k = char_of_int ((int_of_char k) land 0x1F)
let sigint_handler fd sg = let _ = Unix.write fd (String.make 1 (ctrl 'C')) 0 1 in ()

let _ =
  at_exit quit_message;
  let terminfo = Unix.tcgetattr Unix.stdin in
  let new_terminfo = {terminfo with Unix.c_icanon = false; Unix.c_vmin = 0; Unix.c_vtime = 0; Unix.c_echo = false } in
  let reset_stdin () = Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH terminfo in
    at_exit reset_stdin;
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH new_terminfo;
  Sys.set_signal Sys.sigchld (Sys.Signal_handle exit);
  match Ptyutils.forkpty () with 
    | (-1, _, _) -> failwith "Error"
    | (0, _, _) -> child ()
    | (pid, fd, _) ->
          Sys.set_signal Ptyutils.sigwinch (Sys.Signal_handle (winch pid));
          Sys.set_signal Sys.sigint (Sys.Signal_handle (sigint_handler fd));
        let close_fd _ = Unix.close fd in at_exit close_fd;
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
            if (List.mem Unix.stdout output) then
              begin
                if !need_reprint then
                  let ws = Ptyutils.get_winsize Unix.stdout in Ptyutils.set_winsize fd ws;
                  let _ = Ringbuffer.write Unix.stdout buffer in read_len := 0; need_reprint := false
                  else
                    let _ = Unix.write Unix.stdout out_buffer 0 !read_len in read_len := 0
              end;
            if List.mem Unix.stdin input then
              input_len := !input_len + (Unix.read Unix.stdin in_buffer 0 in_buffsize);
            if List.mem fd output then
              let _ = Unix.write fd in_buffer 0 !input_len in input_len := 0
        done
