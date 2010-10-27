let find_line_rev rb width len =
  let rec find i =
    if (Ringbuffer.get rb i = '\n') then
      begin
        let i' = i + 1 in
          if len - i' > width then
            (i' + width, Ringbuffer.substring_of_ringbuffer rb (i' + width) (len - (i' + width)))
          else
            (i, Ringbuffer.substring_of_ringbuffer rb i' (len - i'))
      end
    else
      begin
        if i = 0 then
          begin
            if len - i > width then
              (i + width, Ringbuffer.substring_of_ringbuffer rb (i + width) (len - (i + width)))
            else
              (i, Ringbuffer.substring_of_ringbuffer rb i (len - i))
          end
        else
          find (i - 1)
      end
  in
    find (len - 1)

let find_newline_rev rb len =
  let rec find i =
    if (Ringbuffer.get rb i = '\n') then
      (i, Ringbuffer.substring_of_ringbuffer rb (i+1) (len - (i+1)))
    else
      begin
        if (i = 0) then
          (i, Ringbuffer.substring_of_ringbuffer rb i (len - i))
        else
          find (i - 1)
      end
  in
    find (len - 1)

let process rb rows width =
  let rec pr len row acc = match (len, row) with
    | (l, _) when l <= 0 -> acc
    | (_, 0) -> acc
    | _ -> let (l, str) = find_line_rev rb width len in
        pr l (row-1) (str::acc)
  in pr (Ringbuffer.used_length rb) rows []

