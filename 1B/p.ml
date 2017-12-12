
type numeration =
  | Excel of string * int
  | Coord of int * int

let string_to_numeration s =
  if Str.string_match (Str.regexp "^\\([A-Z]+\\)\\([1-9][0-9]*\\)$") s 0 then
    Some(Excel (Str.matched_group 1 s, int_of_string (Str.matched_group 2 s)))
  else if Str.string_match (Str.regexp "^R\\([1-9][0-9]*\\)C\\([1-9][0-9]*\\)$") s 0 then
    Some(Coord (int_of_string (Str.matched_group 1 s), int_of_string (Str.matched_group 2 s)))
  else
    None

let string_to_list s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in exp (String.length s - 1) []

let pos_in_alphabet = function
  | 'A' -> 1  | 'B' -> 2  | 'C' -> 3  | 'D' -> 4  | 'E' -> 5
  | 'F' -> 6  | 'G' -> 7  | 'H' -> 8  | 'I' -> 9  | 'J' -> 10
  | 'K' -> 11 | 'L' -> 12 | 'M' -> 13 | 'N' -> 14 | 'O' -> 15
  | 'P' -> 16 | 'Q' -> 17 | 'R' -> 18 | 'S' -> 19 | 'T' -> 20
  | 'U' -> 21 | 'V' -> 22 | 'W' -> 23 | 'X' -> 24 | 'Y' -> 25
  | 'Z' -> 26 | _ -> -1

let char_at_pos = function
  | 1  -> 'A' | 2  -> 'B' | 3  -> 'C' | 4  -> 'D' | 5  -> 'E'
  | 6  -> 'F' | 7  -> 'G' | 8  -> 'H' | 9  -> 'I' | 10 -> 'J'
  | 11 -> 'K' | 12 -> 'L' | 13 -> 'M' | 14 -> 'N' | 15 -> 'O'
  | 16 -> 'P' | 17 -> 'Q' | 18 -> 'R' | 19 -> 'S' | 20 -> 'T'
  | 21 -> 'U' | 22 -> 'V' | 23 -> 'W' | 24 -> 'X' | 25 -> 'Y'
  | 0 -> 'Z' | x  -> ' '

let string_to_column_number s =
  let rec aux acc cur = function
    | [] -> acc
    | h :: t -> aux ((pos_in_alphabet h)*int_of_float(26. ** (float_of_int cur)) + acc) (cur+1) t
  in aux 0 0 (List.rev (string_to_list s))

let column_number_to_string col =
  let rec aux acc last_div n =
    if n <= 26
    then (
      if not last_div && ((String.length acc > 0) && ((String.get acc 0) = 'Z')) && (char_at_pos ((n-1) mod 26)) == 'Z'
      then acc
      else if last_div || ((String.length acc > 0) && ((String.get acc 0) = 'Z'))
      then (String.make 1 (char_at_pos ((n-1) mod 26))) ^ acc
      else (String.make 1 (char_at_pos (n mod 26))) ^ acc
    )
    else
      if last_div || ((String.length acc > 0) && ((String.get acc 0) = 'Z'))
      then aux (String.make 1 (char_at_pos ((n-1) mod 26)) ^ acc ) ((n mod 26) = 0) (n / 26)
      else aux (String.make 1 (char_at_pos (n mod 26)) ^ acc ) ((n mod 26) = 0) (n / 26)
  in aux "" false col

let numeration_to_string = function
  | Excel (col, row) -> col ^ (string_of_int row)
  | Coord (row, col) -> "R" ^ (string_of_int row) ^ "C" ^ (string_of_int col)

let switch_numeration = function
  | Excel (col, row) -> Coord (row, string_to_column_number(col))
  | Coord (row, col) -> Excel (column_number_to_string(col), row)

(*
let spreadsheets l =
  l
  |> List.map string_to_numeration
  |> List.filter (function Some(_) -> true | None -> false)
  |> List.map (function Some(x) -> x | x -> assert false)
  |> List.map switch_numeration
  |> List.map numeration_to_string
*)

let spreadsheets l =
  l
  |> List.map (fun x ->
      match string_to_numeration (x) with
      | Some(e) -> numeration_to_string (switch_numeration e)
      | None -> assert false)

let () =
  let num = read_int () in
  let do_parse cnt =
    let rec loop acc = function
      | c when c < 1 -> acc
      | c ->
        let s = Scanf.bscanf Scanf.Scanning.stdin "%s\n" (fun x -> x)
        in loop (s::acc) (c-1)
    in List.rev (loop [] cnt)
  in List.iter (Printf.printf "%s\n") (spreadsheets (do_parse num))

