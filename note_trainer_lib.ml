(* open Tezos_clic *)
open Tezos_error_monad
open Error_monad
open Tezos_clic

(* A module theory
     A module Note
       type t : a representation of the note

       of_string : string -> t option

       From a string representation of a note to a note.
       In the current version, we do not consider flats.
       The following names are used:
         C C# D D# E F F# G G# A A# B

       Returns None if the argument does not correspond figure in
       the list above.

       to_string : t -> string

         Converts a note to a string using the scheme above

       pp : fmt -> t -> ??

       Pretty-prints a note

       transpose : t -> int -> t

       [transpose n i] transposes the note [n] by [i] semi-tones. If [i] is negative,
       the note is transposed down.

       of_int : int -> t

       [of_int x] gives the xth note on the note circle,
       where 0 corresponds to C, 1 to C#, etc.
       where 12 corresponds to C again
       where -1 correspond to B, -2 to A#, etc.

       arbitrary : Random.state -> (t * Random.state)

       [arbitrary s] returns a random note using the state of the
       PRNG in [s]. It also returns the new state of the PRNG.
 *)
module Theory = struct
  module Note = struct
    type t = int

    let notes =
      [| "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B" |]

    let of_string : string -> t option =
     fun s ->
      let found = ref None in
      Array.iteri (fun i n -> if n = s then found := Some i else ()) notes ;
      !found

    let to_string : t -> string =
     (* -1 % 12 = 11 *)
     fun i ->
      let nmod i j =
        let r = i mod j in
        if r < 0 then j + r else r
      in
      notes.(nmod i (Array.length notes))

    let pp fmt n = Format.pp_print_string fmt (to_string n)
  end
end

(*
   A module Questions
     type t

   the state of a module Questions. It consists of a Random.State.t

     init : ~seed:int -> t

   the function in receives a [seed]. It is used to initalize the state
   of the Questions using Random.State.make.

     next_question : t -> (string, string) -> t

   the function init initalize the internal state of the Questions module.
   It is composed of a PRNG state. It is initalized by the seed parameter.

   The function [next_answer t] generates a random question, and returns the
   question and the expected answer as a string.

   Possible questions are:
     Note [note] transposed up [interval] semi-times gives what?

     To generate a (question, answer), we :
        generate a random note by calling Note.arbitrary
        generate a random interval by calling Random.State.int and applying modulo 12
        then the question is "Note [note] transposed up [interval] semi-times gives what? "
        and the answer is [Note.transpose note interval]

*)

(* type error += Test_error *)

(*
   type config :
     seed : int

   the seed is used to initalize the PRNG of Questions that generates questions
*)
type config = { seed : int }

(* [default_config ()] returns the default configuration.
   It sets the seed to the current pid.
*)
let default_config : unit -> config = fun _ -> { seed = Unix.getpid () }

(*
   A module Interaction:
     run : Questions.t -> unit Lwt.t

   The function runs an interactive loop.

   Each iteration:
    1. gets a (question, answer), by calling Questions.next_question
    1. Echo the question
    2. Reads response from prompt.
       - If response is "^D", quit loop with result unit
       - If response is answer, the response "Correct!\r" and re-iterate
       - If response is not answer, the response is "False, try again!".
         Re-ask the question until the correct answer is obtained.
*)

(* A function read_parameters

   The only allowed parameter is "--seed" which takes an integer. This
   parameter is packed into the configuration and returned.

   If "--seed" is missing, then the value from ~default is used instead.

   If "--seed" is present, but not an integer, then the error Invalid_parameter
   is returned.

   Any other trailing strings results in an Invalid_parameter.  *)

type error += Invalid_parameter

let read_parameters : default:config -> string list -> config tzresult Lwt.t =
 (* Clic.parse_global_options *)
 fun ~default args ->
  let param =
    Clic.parameter (fun _ s ->
        match Stdlib.int_of_string_opt s with
        | Some i -> return i
        | None -> fail Invalid_parameter)
  in
  let options =
    Clic.args1 @@ Clic.arg ~doc:"Seed" ~long:"seed" ~placeholder:"N" param
  in
  Clic.parse_global_options options () args >>=? fun res ->
  match res with
  | (seed, []) ->
      let seed = Option.value seed ~default:default.seed in
      return { seed }
  | _ -> fail Invalid_parameter

let () = ()
