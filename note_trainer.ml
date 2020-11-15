(* Description:

   v0.1:

   while true:
     ask "what is [note] transed up / down X semi-tones?"

   - should use readline or similar ?
   - try first with prompt

   - testing:
      - use inline ppx
      - use alcotest
      - use crowbar
      - use cram

   v0.2:


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

let () = ignore (Note_trainer_lib.read_parameters [])
