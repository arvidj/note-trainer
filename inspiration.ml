module StrMap = Map.Make (String)

let chords =
  [| "A"; "A#"; "B"; "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#" |]

let rev_chords =
  StrMap.empty
  |> StrMap.add_seq
       (Array.to_seq @@ Array.mapi (fun i chord -> (chord, i)) chords)

let transpose n a = (a + n + Array.length chords) mod Array.length chords

let print_transposer () =
  let transposer = Random.int 24 - 12 in
  let chord = Random.int @@ Array.length chords in
  Format.printf "What's %s transposed %s by %d?@.%!" chords.(chord)
    (if transposer > 0 then "up" else "down")
    (Int.abs transposer);
  transpose chord transposer

let loop () =
  let rec loop expected_chord =
    Format.printf "> %!";
    match read_line () with
    | "exit" -> exit 0
    | "reveal" ->
        Format.printf "%d@." expected_chord;
        Format.printf "It was %s.@." chords.(expected_chord);
        loop (print_transposer ())
    | x when not @@ Array.mem x chords ->
        Format.printf "Please enter a valid chord.@.";
        loop expected_chord
    | chord ->
        if StrMap.find chord rev_chords == expected_chord then (
          Format.printf "yeah!@.";
          loop (print_transposer ()) )
        else (
          Format.printf "nope! Try again please.@.";
          loop expected_chord )
  in
  loop @@ print_transposer ()

let _ =
  ( match int_of_string_opt Sys.argv.(1) with
  | None ->
      Format.printf "Incorrect seed. The seed must be a number";
      exit 1
  | Some i -> Random.init i
  | exception Invalid_argument _ ->
      Format.printf "No seed input. Initializing with a random seed.@.";
      Random.self_init () );
  loop ()
