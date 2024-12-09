let is_valid (fst: int) (snd: int list): bool =
  let rec aux tar lst = match lst with
    | [x] -> tar = x
    | n1 :: n2 :: tl -> aux fst (n1+n2 :: tl) || aux fst (n1*n2 :: tl)
    | _ -> false
    in
    aux fst snd

let concat_valid (fst: int) (snd: int list): bool =
  let rec aux tar lst = match lst with
    | [x] -> tar = x
    | n1 :: n2 :: tl -> aux fst (n1+n2 :: tl) || aux fst (n1*n2 :: tl) || aux fst (int_of_string((string_of_int n1) ^ (string_of_int n1)) :: tl)
    | _ -> false
  in
  aux fst snd

let mapper (entry: string list) (valid: int -> int list -> bool): int =
  let fst = int_of_string (List.nth entry 0) in
  let snd = List.nth entry 1
    |> String.split_on_char ' '
    |> List.filter (fun x -> x <> "")
    |> List.map (int_of_string)
  in
  if valid fst snd then
    fst
  else
    0

let solve (contents: string): int =
  String.split_on_char '\n' contents
    |> List.map (String.split_on_char ':')
    |> List.map (fun x -> mapper x is_valid)
    |> List.fold_left (+) 0

let main () =
  let ch = open_in "inputs/day07.in" in
  let contents = really_input_string ch (in_channel_length ch) in
  print_int (solve contents);
  print_endline ""
