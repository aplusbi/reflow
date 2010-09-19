let rec really_read fd str start len =
  if len <= 0 then ()
  else
    try
    let read_len = Unix.read fd str start len in
      really_read fd str (start+read_len) (len-read_len)
    with _ -> ()
 
let child _ = Unix.execvp "yes" [|"yes"; "hello"|]

let buffer = String.create 256

let _ =
  Printf.printf "Parent %d\n" (Unix.getpid ());
  match Ptyutils.forkpty_nocallback None None with
    | (-1, _, _) -> failwith "Error"
    | (0, _, _) -> child ()
    | (pid, fd, name) -> while true do
        Unix.write Unix.stdout "Parent with child" 0 17;
        try
          let read_len = Unix.read fd buffer 0 256 in
            if read_len > 0 then
              Printf.printf "%d %s" read_len buffer
            else ()
        with _ -> Printf.printf "Couldn't read\n";
                  Unix.sleep 2
      done
