let is_valid_csi_ender = function
  | 'a'..'z' | 'A'..'Z' | '@' | '`' -> true
  | _ -> false

let rec split_on_width str start finish width =
  let rec line i len esc =
    if len >= width || i >= finish then
      let amt1 = finish - start in
      let amt2 = i - start in
      let amt = min amt1 amt2 in
      (i, String.sub str start amt)
    else
      begin
        match esc with
          | true -> if is_valid_csi_ender str.[i] then
              line (i+1) len false
            else
              line (i+1) len true
          | false ->
              begin
                match str.[i] with
                  | '\027' -> line (i+1) len true
                  | '\n' | '\r' -> line (i+1) len false
                  | _ -> line (i+1) (len+1) false
              end
      end
  in
    if start >= finish then
      []
    else
      let (x, l) = line start 0 false in
        l::(split_on_width str x finish width)

let find_newline_rev rb len =
  let rec find i =
    if i != len && (Ringbuffer.get rb i = '\n') then
      (i, Ringbuffer.substring_of_ringbuffer rb (i+1) (1 + len - (i+1)))
    else
      begin
        if (i = 0) then
          (i, Ringbuffer.substring_of_ringbuffer rb i (1 + len - i))
        else
          find (i - 1)
      end
  in
    find len

let process rb rows width =
  let rec pr len row acc = match (len, row) with
    | (l, _) when l <= 0 -> acc
    | (_, 0) -> acc
    | _ -> let (l, str) = find_newline_rev rb len in
        pr l (row-1) ((split_on_width str 0 (String.length str) width) @ acc)
  in
    pr ((Ringbuffer.used_length rb) - 1) rows []

