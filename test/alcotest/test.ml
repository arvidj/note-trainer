let read_parameters_ok _ =
  let default = { seed : 100 } in
  Note_trainer_lib.read_parameters ~default ["--seed"; "0"] >>=? fun { seed } ->
  Alcotest.check Alcotest.int seed 0

let read_parameters_ok1 _ =
  let default = { seed : 100 } in
  Note_trainer_lib.read_parameters ~default ["--seed"; "123"] >>=? fun { seed } ->
  Alcotest.check Alcotest.int seed 123

let read_parameters_default _ =
  let default = { seed : 100 } in
  Note_trainer_lib.read_parameters ~default [] >>=? fun { seed } ->
  Alcotest.check Alcotest.int seed 100

let read_parameters_trailing _ =
  let default = { seed : 100 } in
  Note_trainer_lib.read_parameters ~default ["foo"] >>=
    function
    | Error _ -> Alcotest.pass
    | Ok _ -> Alcotest.fail "read_parameters_trailing should not admit
                             trailing parameters"

let tests = []

let () =
  Alcotest.run
    "notetrainer"
    [("read_parameters", [("read_parameters_ok", `Quick, read_parameters_ok)])]
