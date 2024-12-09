
let parse (contents: string): 

let solve (contents: string): int =
  let res = parse contents in

let main (): unit =
  let ch = open_in "inputs/day09.in" in
  let contents = really_input_string ch (in_channel_length ch) in
  solve_a contents