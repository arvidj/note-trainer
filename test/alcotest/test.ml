(** { [Theory.note] tests } *)

open Note_trainer_lib
open Theory.Note

let notes =
  [| "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B" |]

let test_of_string_opt () =
  Array.iter
    (fun nstr ->
      match of_string_opt nstr with
      | Some n -> Alcotest.check Alcotest.string "round-trip" nstr (to_string n)
      | None -> Alcotest.fail "could not transform from string")
    notes

let test_pp () =
  Array.iter
    (fun nstr ->
      let to_string_fmt n = Format.asprintf "%a" pp n in
      match of_string_opt nstr with
      | Some n ->
          Alcotest.check Alcotest.string "round-trip fmt" nstr (to_string_fmt n)
      | None -> Alcotest.fail "could not transform from string")
    notes

let test_of_int () =
  let ( -- ) i j = List.init (j - i + 1) (fun x -> x + i) in
  let note_testable = Alcotest.testable (Fmt.of_to_string to_string) eq in
  Alcotest.check
    Alcotest.(array note_testable)
    "of_int"
    (Array.map (fun n -> of_string_opt n |> Option.get) notes)
    (Array.of_list (List.map of_int (0 -- 11)))

let note_arbitrary = QCheck.map ~rev:to_int of_int QCheck.int

let test_transpose_order =
  QCheck.Test.make
    ~name:"Note.Theory.transpose is commutative"
    ~count:1000
    (QCheck.triple note_arbitrary QCheck.int QCheck.int)
    (fun (n, i, j) ->
      let t2 n i j = transpose (transpose n i) j in
      t2 n i j = t2 n j i)

let test_transpose_wraps =
  QCheck.Test.make
    ~name:"Note.Theory.transpose transposing by octave is identity"
    ~count:1000
    (* small int : this does not hold for ints i such that n + i*12 oer) *)
    (QCheck.pair note_arbitrary QCheck.small_int)
    (fun (n, i) -> eq n (transpose n (i * 12)))

(** { [default_parameters] tests } *)
let test_default_config () =
  let pid = Unix.getpid () in
  let { seed } = default_config () in
  Alcotest.check Alcotest.int "expect <pid>" pid seed

let prop_tests =
  List.map
    QCheck_alcotest.to_alcotest
    [test_transpose_wraps; test_transpose_order]

let unit_tests =
  [ ("test_of_string_opt", `Quick, test_of_string_opt);
    ("test_pp", `Quick, test_pp);
    ("test_of_int", `Quick, test_of_int);
    ("test_default_config", `Quick, test_default_config) ]

let () =
  Alcotest.run "notetrainer" [("read_parameters", unit_tests @ prop_tests)]
