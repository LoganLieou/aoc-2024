(** Split on whitespace *)
let on_whitespace (s: string): string list =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' s)

(** Calculate the diff between the sorted arrays *)
let rec diff (lst1: int list) (lst2: int list) (acc: int): int =
  match lst1, lst2 with
  | h :: t, h2 :: t2 -> diff t t2 (acc + (abs(h2 - h)))
  | [], [] -> acc

  (** Note how these cases will not occur since the arrays are equal length *)
  | h :: t, [] -> acc
  | [], h :: t -> acc

let parse (contents: string): (int list * int list) =
  let lines = String.split_on_char '\n' contents in
  let rec loop lines lst1 lst2 = match lines with
    | h :: t ->
      let items = on_whitespace h in
      loop t (lst1 @ [int_of_string @@ List.nth items 0]) (lst2 @ [int_of_string @@ List.nth items 1])
    | [] -> lst1, lst2
  in
  loop lines [] []

(** Solves part a *)
let resolve_a (contents: string): int =
  let lst1, lst2 = parse(contents) in
  diff (List.sort compare lst1) (List.sort compare lst2) 0

(** Solves part b *)
let resolve_b (contents: string): int =
  let freq = Hashtbl.create 1000 in
  let lst1, lst2 = parse(contents) in
  let rec loop lst = match lst with
    | h :: t ->
      let v = Hashtbl.find_opt freq h in
      if v = None then Hashtbl.add freq h 1 else Hashtbl.add freq h ((Option.get v) + 1);
      loop t
    | [] -> ()
  in
  loop lst2;
  List.fold_left (fun acc x ->
    let v = Hashtbl.find_opt freq x in
    if v = None then acc else acc + (Option.get v * x)
  ) 0 lst1

let main (): unit =
  let ch = open_in "inputs/day01a.in" in
  let contents = really_input_string ch (in_channel_length ch) in
  print_int (resolve_a contents);
  print_endline "";
  print_int (resolve_b contents);
  print_endline "";
