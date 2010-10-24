type character = Ch of int
  | Color of int * int
  | Attr of int

let is_control c = (c land 0x60) = 0

let interpret_csi_sgr params =
  let attrs = ref 0 in
  let fg = ref 0 in
  let  bg = ref 0 in
  let check_param = function
    | 1 -> attrs := !attrs lor Curses.A.bold
    | x when x >= 30 && x <= 37 -> fg := x - 30
    | x when x >= 40 && x <= 47 -> bg := x - 40
    | _ -> ()
  in List.iter check_param params;
     let return = if !fg != 0 || !bg != 0 then [Color(!fg, !bg)] else [] in
       Some(if !attrs != 0 then Attr(!attrs)::return else return)

let interpret_csi = function [] -> None
  | h::t -> 
    let rec to_digits b d acc = function [] -> acc
      | x::xs when x = ';' -> to_digits 1 0 (d::acc) xs
      | x::xs when x >= '0' && x <='9' -> to_digits (b * 10) (d + (((int_of_char x) - (int_of_char '0')) * b)) acc xs
      | _::xs -> to_digits b d acc xs
    in
    let params = to_digits 1 0 [] t in
      match h with
        | 'm' -> interpret_csi_sgr params
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
            | x -> Ringarray.write_single (Ch x) out_buff
        end
  in 
    for i = 0 to (length buff)-1 do
      process i
    done

