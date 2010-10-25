type character = Ch of int
  | Color of int * int
  | Attr of int
  | Attroff
  | Newline

let is_control c = (c land 0x60) = 0

let interpret_csi_sgr params =
  let attrs = ref (-1) in
  let fg = ref 0 in
  let  bg = ref 0 in
  let check_param = function
    | 0 -> attrs := 0
    | 1 -> attrs := Curses.A.bold
    | x when x >= 30 && x <= 37 -> fg := x - 30
    | x when x >= 40 && x <= 47 -> bg := x - 40
    | _ -> ()
  in List.iter check_param params;
     let return = if !fg != 0 || !bg != 0 then [Color(!fg, !bg)] else [] in
       match !attrs with 
       | -1 -> return
       | 0 -> Attroff::return
       | a -> Attr(a)::return

let interpret_csi = function [] -> None
  | h::t -> 
    let rec to_digits b d acc = function [] -> d::acc
      | x::xs when x >= '0' && x <='9' -> to_digits (b * 10) (d + (((int_of_char x) - (int_of_char '0')) * b)) acc xs
      | _::xs -> to_digits 1 0 (d::acc) xs
    in
    let params = to_digits 1 0 [] t in
      match h with
        | 'm' -> Some(interpret_csi_sgr params)
        | _ -> None

let is_valid_csi_ender = function
  | 'a'..'z' | 'A'..'Z' | '@' | '`' -> true
  | _ -> false

let process_escape = function [] -> None
  | _::[] -> None
  | h::t as l -> match List.nth t ((List.length t) - 1) with
      | '[' when is_valid_csi_ender h -> interpret_csi l
      | _ -> None

let process_buffer get length buff out_buff =
  let esc = ref false in
  let ebuff = ref [] in
  let process i =
    let c = get buff i in
    match !esc with
      | true -> begin
          ebuff := c::(!ebuff);
          match process_escape !ebuff with None -> ()
            | Some(l) -> esc := false; ebuff := [];
                         List.iter (fun x -> Ringarray.write_single x out_buff) l
        end
      | false -> begin
          match int_of_char c with
            | 0o33 -> esc := true
            | 13 -> Ringarray.write_single Newline out_buff
            | 10 -> ()
            | x -> Ringarray.write_single (Ch x) out_buff
        end
  in 
    for i = 0 to (length buff)-1 do
      process i
    done

let tail n l = 
  let rec remove = function (_, []) -> []
    | (0, ls) -> ls
    | (x, h::t) -> remove ((x-1), t)
  in
  let len = (List.length l) - n in
    if len < 0 then l else remove (len, l)

let get_valid_lines num_lines width buff =
  let rec add_blanks n l = match n with 0 -> l
    | _ -> add_blanks (n-1) (Ch(int_of_char ' ')::l)
  in
  let len = Ringarray.used_length buff in
  let rec process = function 
    | (i, _, l) when i >= len -> (List.rev l)::[]
    | (i, 0, l) -> (List.rev (Newline::l))::(process (i, width, []))
    | (i, w, l) ->
        begin
          match Ringarray.get buff i with
            | Newline -> (List.rev (Newline::(add_blanks (w-1) l)))::(process (i+1, width, []))
            | Ch(_) as c -> process (i+1, w-1, c::l)
            | a -> process (i+1, w, a::l)
        end
  in
      tail num_lines (process (0, width, []))

