module Integer =
struct
  type t = int
  let compare = compare
end
module SigMap = Map.Make(Integer)

let buffsize = 65536
let in_buffsize = 1024
let out_buffsize = 4096
let in_buffer = String.create in_buffsize
let out_buffer = String.create out_buffsize
let out_array = Ringarray.make 32768 (Cursesutils.Ch 0)
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
  let _ = Curses.start_color () in
  Sys.set_signal Sys.sigchld (Sys.Signal_handle exit)

let color_num = ref 1
let print_curses = function Cursesutils.Ch(c) -> ignore (Curses.addch c)
  | Cursesutils.Attr(a) -> Curses.attron a
  | Cursesutils.Color(fg, bg) -> ignore (Curses.init_pair !color_num fg bg); Curses.attron (Curses.A.color_pair !color_num); incr color_num
  | Cursesutils.Attroff -> Curses.attrset 0
  | Cursesutils.Newline -> let y, x = Curses.getyx (Curses.stdscr ()) in ignore (Curses.move (y+1) 0)

let log_curses fd = function Cursesutils.Ch(c) -> ignore (Unix.write fd (String.make 1 (char_of_int c)) 0 1)
  | Cursesutils.Attr(a) -> let str = "$Attr " ^ (string_of_int a) ^ "$" in ignore (Unix.write fd str 0 (String.length str))
  | Cursesutils.Color(fg, bg) -> let str = "@Color " ^ (string_of_int fg) ^ "," ^ (string_of_int bg) ^ "@" in ignore (Unix.write fd str 0 (String.length str))
  | Cursesutils.Attroff -> let str = "$Attr 0$" in ignore (Unix.write fd str 0 (String.length str))
  | Cursesutils.Newline -> ignore (Unix.write fd "\n" 0 1)

let _ =
        let log_fd = Unix.openfile "logging.txt" [Unix.O_WRONLY;  Unix.O_CREAT; Unix.O_TRUNC] 0o666 in at_exit (fun _ -> Unix.close log_fd);
  setup ();
  match Ptyutils.forkpty () with 
    | (-1, _, _) -> failwith "Error"
    | (0, _, _) -> child ()
    | (pid, fd, _) ->
        setup_terminal pid fd;
        let input_len = ref 0 in
        while true do
          let (input, output, _) = Unix.select [Unix.stdin; fd] [Unix.stdout; fd] [] (-1.) in
            if List.mem fd input then
              begin
              let rd_l = (Unix.read fd out_buffer 0 out_buffsize) in
                Cursesutils.process_buffer String.get (fun x -> rd_l) out_buffer out_array;
                if rd_l > 0 then ignore (Unix.write log_fd "*****\n\n" 0 7);Ringarray.iter (log_curses log_fd) out_array
              end;
            (if (List.mem Unix.stdout output) then
              begin
                if !need_reprint then
                  let (w, h) = Curses.get_size () in let ws = {(Ptyutils.get_winsize fd) with Ptyutils.ws_row=h; Ptyutils.ws_col=w} in Ptyutils.set_winsize fd ws
                  else
                          color_num := 1; Ringarray.iter print_curses out_array; 
              end);
            (if List.mem Unix.stdin input then
              begin
                let c = char_of_int (Curses.getch ()) in in_buffer.[!input_len] <- c; input_len := !input_len + 1
              end);
            (if List.mem fd output then
              begin
                let _ = Unix.write fd in_buffer 0 !input_len in input_len := 0
              end);
            let _ = Curses.refresh () in ignore (Curses.move 0 0);
        done
