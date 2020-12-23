
let join sep str_list =
  let buf = Buffer.create 8 in
  let rec loop = function
    | [] -> ()
    | [str] -> Buffer.add_string buf str
    | str :: str_list ->
      Buffer.add_string buf str;
      Buffer.add_string buf sep;
      loop str_list
  in
  loop str_list;
  Buffer.contents buf