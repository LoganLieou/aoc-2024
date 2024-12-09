(** utility function *)
let not_empty (lst: 'a list): bool =
  List.length lst != 0

(** Splitting on whitespace *)
let on_whitespace (s: string): string list =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' s)

(** Returns if a given report is safe *)
let safe (report: int list): bool =
  let rec inc lst = match lst with
    | h1 :: h2 :: t ->
      if h1 < h2 then inc (h2 :: t)
      else false
    | h1 :: [] -> true
    | [] -> true
  in
  let rec dec lst = match lst with
    | h1 :: h2 :: t ->
      if h1 > h2 then dec (h2 :: t)
      else false
    | h1 :: [] -> true
    | [] -> true
  in
  let rec loop lst = match lst with
    | h1 :: h2 :: t ->
      if (abs @@ h1 - h2) <= 3 && (abs @@ h1 - h2) != 0 then
        loop (h2::t)
      else
        false
    | h1 :: [] -> true
    | [] -> true
  in
  (inc report || dec report) && loop report

(** Compute how far away a list is from being safe *)
let rec off_by_one (report: int list): bool =
  let remove_at i lst =
    let rec aux idx acc = function
      | [] -> List.rev acc
      | h :: t ->
          if idx = i then aux (idx + 1) acc t
          else aux (idx + 1) (h :: acc) t
    in
    aux 0 [] lst
  in
  if safe report then
    true
  else
    List.filteri (fun i x -> if safe(remove_at i report) then true else false) report
    |> not_empty

(** Report = line, Level = column *)
let parse (s: string): int list list =
  let lines = String.split_on_char '\n' s in
  List.map (fun x -> List.map (int_of_string) (on_whitespace x)) lines

(** Solves the problem for part A *)
let resolve_a (contents: string): int =
  let reports = parse(contents) in
  List.fold_left (fun acc x -> 
    if safe (x) then acc + 1 else acc
  ) 0 reports

(** Solves the problem for part B *)
let resolve_b (contents: string): int =
  let reports = parse contents in
  List.fold_left (fun acc x -> 
    let v = off_by_one x in
    if v then acc + 1 else acc
  ) 0 reports

let main (): unit =
  let ch = open_in "inputs/day02.in" in
  let contents = really_input_string ch (in_channel_length ch) in
  print_int (resolve_a contents);
  print_endline "";
  print_int (resolve_b contents);
  print_endline "";