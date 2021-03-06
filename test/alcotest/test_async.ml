open Tezos_error_monad
open Error_monad
open Note_trainer_lib

let ( >>=?? ) m f =
  m >>= function
  | Ok v -> f v
  | Error error ->
      Format.printf "Error:\n   %a\n" pp_print_error error ;
      Format.print_flush () ;
      Lwt.return_unit

(** { [default_parameters] tests } *)
let test_default_config _ =
  let pid = Unix.getpid () in
  let { seed } = default_config () in
  Alcotest.check Alcotest.int "expect <pid>" pid seed

(** { [read_parameters] tests } *)
let (default : config) = { seed = 100 }

let test_read_parameters_ok _ =
  read_parameters ~default ["--seed"; "0"] >>=?? fun { seed } ->
  Alcotest.check Alcotest.int "expect 0" 0 seed ;
  Lwt.return_unit

let test_read_parameters_ok1 _ =
  read_parameters ~default ["--seed"; "123"] >>=?? fun { seed } ->
  Alcotest.check Alcotest.int "expect 123" 123 seed ;
  Lwt.return_unit

let test_read_parameters_default _ =
  read_parameters ~default [] >>=?? fun { seed } ->
  Alcotest.check Alcotest.int "expect default" default.seed seed ;
  Lwt.return_unit

let test_read_parameters_trailing _ =
  read_parameters ~default ["foo"] >>= function
  | Error _ -> Lwt.return_unit
  | Ok _ ->
      Alcotest.fail
        "read_parameters_trailing should not admit\n\
        \                             trailing parameters"

let tests_lwt =
  [ ("read_parameters_ok", `Quick, test_read_parameters_ok);
    ("read_parameters_ok1", `Quick, test_read_parameters_ok1);
    ("read_parameters_default", `Quick, test_read_parameters_default);
    ("read_parameters_trailing", `Quick, test_read_parameters_trailing) ]

let () =
  Lwt_main.run
    (Alcotest_lwt.run "notetrainer_lwt" [("notetrainer_lwt", tests_lwt)])
