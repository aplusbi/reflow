let child _ = Unix.execv "/bin/bash" [|"/bin/bash"|]

let buffsize = 65536
let in_buffsize = 1024
let buffer = Ringbuffer.create buffsize
let in_buffer = String.create in_buffsize
let need_reprint = ref false

let winch _ = need_reprint := true

let setup_signal_passing pid =
  Sys.set_signal Sys.sighup (Sys.Signal_handle (Unix.kill pid));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (Unix.kill pid));
  Sys.set_signal Sys.sigcont (Sys.Signal_handle (Unix.kill pid));
  Sys.set_signal Sys.sigtstp (Sys.Signal_handle (Unix.kill pid))

let _ =
  Sys.set_signal Sys.sigchld (Sys.Signal_handle exit);
  Sys.set_signal (Ptyutils.sigwinch ()) (Sys.Signal_handle winch);
  match Ptyutils.forkpty_nocallback None None with
    | (-1, _, _) -> failwith "Error"
    | (0, _, _) -> child ()
    | (pid, fd, _) ->
        let close_fd _ = Unix.close fd in at_exit close_fd;
        let read_len = ref 0 in
        let input_len = ref 0 in
        while true do
          let (input, output, _) = Unix.select [Unix.stdin; fd] [Unix.stdout; fd] [] (-1.) in
            if List.mem fd input then
              read_len := !read_len + (Ringbuffer.read fd buffer);
            if (List.mem Unix.stdout output) then
              begin
                if !need_reprint then
                  let _ = Ringbuffer.write Unix.stdout buffer in read_len := 0; need_reprint := false
                  else
                    let _ = Ringbuffer.writebytes Unix.stdout buffer (-(!read_len)) !read_len in read_len := 0
              end;
            if List.mem Unix.stdin input then
              input_len := !input_len + (Unix.read Unix.stdin in_buffer 0 in_buffsize);
            if List.mem fd output then
              let _ = Unix.write fd in_buffer 0 !input_len in input_len := 0
        done
