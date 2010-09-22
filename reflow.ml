let rec really_read fd str start len =
  if len <= 0 then ()
  else
    try
    let read_len = Unix.read fd str start len in
      really_read fd str (start+read_len) (len-read_len)
    with _ -> ()
 
let child _ = Unix.execv "/bin/bash" [|"/bin/bash"|]

let buffsize = 65536
let in_buffsize = 1024
let buffer = String.create buffsize
let in_buffer = String.create in_buffsize

let _ =
  let pos = ref 0 in
  Sys.set_signal Sys.sigchld (Sys.Signal_handle exit);
  match Ptyutils.forkpty_nocallback None None with
    | (-1, _, _) -> failwith "Error"
    | (0, _, _) -> child ()
    | (_, fd, _) -> while true do
        let input_len = Unix.read Unix.stdin in_buffer 0 in_buffsize in
        let _ = Unix.write fd in_buffer 0 input_len in
          Unix.sleep 1;
        let loop = ref true in
          while !loop do
            if !pos >= buffsize then pos := 0;
            try
              let read_len = Unix.read fd buffer !pos (buffsize - !pos) in
              let _ = Unix.write Unix.stdout buffer !pos read_len in
              if read_len < (buffsize - !pos) then loop := false;
              pos := !pos + read_len
            with _ -> loop := false
          done
      done
