type 'a t = { buffer: 'a array; mutable curr: int; length: int; mutable full: bool };;

let make len x = {buffer = Array.make len x; curr = 0; length = len; full = false};;

let rec write arr rb = match (rb.length - rb.curr) with
  | amt when amt <= 0 -> rb.curr <- 0; rb.full <- true; write arr rb
  | amt ->
      let rec wr start =
        let length = (Array.length arr) - start in
        let real_len = min length amt in
          Array.blit arr start rb.buffer rb.curr real_len; rb.curr <- rb.curr + real_len;
          if real_len = amt then wr amt
      in wr 0

let rec write_single x rb = if (rb.length - rb.curr) = 0 then
  (rb.curr <- 0; rb.full <- true); rb.buffer.(rb.curr) <- x; rb.curr <- rb.curr + 1

let array_of_ringbuffer rb = match rb.full with
  | false -> Array.sub rb.buffer 0 rb.curr 
  | true -> Array.append (Array.sub rb.buffer rb.curr (rb.length - rb.curr)) (Array.sub rb.buffer 0  rb.curr)

let iter f rb =
  if rb.full then
    for i = rb.curr to rb.length - 1 do
      f rb.buffer.(i)
    done;
  for i = 0 to rb.curr - 1 do
    f rb.buffer.(i)
  done

let fold_left f cont rb =
  let rec acc c i len = if i >= len then c else acc (f c rb.buffer.(i)) (i+1) len in
    match rb.full with
      | true -> let c = acc cont rb.curr rb.length in acc c 0 rb.curr
      | false -> acc cont 0 rb.curr

let get rb i = match rb.full with
  | false -> if i >= rb.curr then
      invalid_arg "Out of range"
    else
      rb.buffer.(i)
  | true -> if i >= rb.length then
      invalid_arg "Out of range"
    else
      let x = i + rb.curr in
        if x < rb.length then rb.buffer.(x)
        else rb.buffer.(x - rb.length)

let length rb = rb.length

let used_length rb = match rb.full with
  | true -> rb.length
  | false -> rb.curr

