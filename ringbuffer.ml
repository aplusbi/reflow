type t = { buffer: string; mutable curr: int; length: int; mutable full: bool };;

let create len = {buffer = String.create len; curr = 0; length = len; full = false};;

let rec unix_read fd rb = match (rb.length - rb.curr) with
  | amt when amt <= 0 -> rb.curr <- 0;
  if not rb.full then
          rb.full <- true;
  unix_read fd rb
  | amt ->
      let read_len = Unix.read fd rb.buffer rb.curr amt in
        rb.curr <-  rb.curr + read_len;
        read_len + (if read_len = amt then unix_read fd rb else 0)

let rec unix_write fd rb = match rb.full with
  | false -> Unix.write fd rb.buffer 0 rb.curr
  | true -> let amt = (rb.length - rb.curr) in
      begin
        match Unix.write fd rb.buffer rb.curr amt with
          | len when len < amt -> len
          | len -> len + (Unix.write fd rb.buffer 0 rb.curr)
      end

let rec read_from_string str rb offset strlen =
  let amt = rb.length - rb.curr in
  let len = min strlen amt in
    String.blit str offset rb.buffer rb.curr len;
    rb.curr <- rb.curr + len;
    if len = amt then let len = strlen - amt in
      rb.curr <- 0; rb.full <- true;
      read_from_string str rb (offset + amt) len

let string_of_ringbuffer rb = match rb.full with
  | false -> let str = String.create rb.curr in String.blit rb.buffer 0 str 0 rb.curr; str
  | true -> let str = String.create rb.length in
      String.blit rb.buffer rb.curr str 0 (rb.length - rb.curr);
      String.blit rb.buffer 0 str (rb.length - rb.curr) rb.curr;
      str

let substring_of_ringbuffer rb start len = match rb.full with
  | false -> String.sub rb.buffer start len
  | true -> let s = rb.curr + start in
    let s' = if s < rb.length then s else s - rb.length in
      if s' + len < rb.length then
        String.sub rb.buffer s' len
      else
        (String.sub rb.buffer s' (rb.length - s')) ^ (String.sub rb.buffer 0 (len - (rb.length - s')))

let iter f rb =
  if rb.full then
    for i = rb.curr to rb.length - 1 do
      f rb.buffer.[i]
    done;
  for i = 0 to rb.curr - 1 do
    f rb.buffer.[i]
  done

let get rb i = match rb.full with
  | false -> if i >= rb.curr then
      invalid_arg "Out of range"
    else
      rb.buffer.[i]
  | true -> if i >= rb.length then
      invalid_arg "Out of range"
    else
      let x = i + rb.curr in
        if x < rb.length then rb.buffer.[x]
        else rb.buffer.[x - rb.length]

let length rb = rb.length

let used_length rb = match rb.full with
  | true -> rb.length
  | false -> rb.curr

