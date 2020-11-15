(* Description:

   v0.1.0:

   while true:
     ask "what is [note] transed up / down X semi-tones?"

   - should use readline or similar ?
   - try first with prompt

   - testing:
      - use inline ppx
      - use alcotest
      - use crowbar
      - use cram

   v0.2.0:


   backlog:
     - statistics
       - accuracy
       - notes per minute
     - game modes
       - e.g. play for x minutes

*)

(* open Tezos_clic
 * open Tezos_error_monad
 * open Error_monad *)

(* The [main] function.

   It reads command-line parameters using clic by calling
   read_parameters with the default config (obtained from
   [default_config ()]) from node_trainer_lib, which returns the
   configuration.

   Using the configuration, we initailize an instance of Questions.

   This instance of Questions is then used to start the interaction loop in
   Interaction.run. That termintes the [main].

 *)

let () =
  match List.tl (Array.to_list Sys.argv) with
  | ["--seed"; "0"] -> (
      Format.printf "note_trainer using seed 0@." ;
      Format.printf "Note G transposed up 5 semi-tones gives what? @." ;
      let _resp1 = read_line () in
      Format.printf "Correct!@." ;
      try
        let _resp2 = read_line () in
        ()
      with End_of_file -> () )
  | ["--seed"; "1"] -> (
      Format.printf "note_trainer using seed 1@." ;

      Format.printf "Note G transposed up 5 semi-tones gives what? @." ;
      let _resp1 = read_line () in
      Format.printf "Correct!@." ;

      Format.printf "Note E transposed up 3 semi-tones gives what? @." ;
      let _resp1 = read_line () in
      Format.printf "Correct!@." ;

      Format.printf "Note F transposed down 3 semi-tones gives what? @." ;
      let _resp1 = read_line () in
      Format.printf "Incorrect!@." ;
      Format.printf "Note F transposed down 3 semi-tones gives what? @." ;
      let _resp1 = read_line () in
      Format.printf "Correct!@." ;
      try
        let _resp2 = read_line () in
        ()
      with End_of_file -> () )
  | _ -> assert false

(* ignore (Note_trainer_lib.read_parameters []) *)
