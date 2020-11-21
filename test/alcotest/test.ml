open Tezos_error_monad
open Error_monad

let ( >>=?? ) m f =
  m >>= function
  | Ok v -> f v
  | Error error ->
      Format.printf "Error:\n   %a\n" pp_print_error error ;
      Format.print_flush () ;
      Lwt.return_unit

let (default : Note_trainer_lib.config) = { seed = 100 }

let read_parameters_ok _ =
  Note_trainer_lib.read_parameters ~default ["--seed"; "0"]
  >>=?? fun { seed } ->
  Alcotest.check Alcotest.int "expect 0" 0 seed ;
  Lwt.return_unit

let read_parameters_ok1 _ =
  Note_trainer_lib.read_parameters ~default ["--seed"; "123"]
  >>=?? fun { seed } ->
  Alcotest.check Alcotest.int "expect 123" 123 seed ;
  Lwt.return_unit

let read_parameters_default _ =
  Note_trainer_lib.read_parameters ~default [] >>=?? fun { seed } ->
  Alcotest.check Alcotest.int "expect default" default.seed seed ;
  Lwt.return_unit

let read_parameters_trailing _ =
  Note_trainer_lib.read_parameters ~default ["foo"] >>= function
  | Error _ -> Lwt.return_unit
  | Ok _ ->
      Alcotest.fail
        "read_parameters_trailing should not admit\n\
        \                             trailing parameters"

let tests =
  [ ("read_parameters_ok", `Quick, read_parameters_ok);
    ("read_parameters_ok1", `Quick, read_parameters_ok1);
    ("read_parameters_default", `Quick, read_parameters_default);
    ("read_parameters_trailing", `Quick, read_parameters_trailing) ]

let () =
  Alcotest_lwt.run "notetrainer" [("read_parameters", tests)] |> Lwt_main.run
