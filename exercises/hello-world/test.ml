open OUnit2
open Hello_world

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
     sk skip;
     assert_equal ~printer:(fun x -> x) exp (got ())

let tests = [
     "Say Hi!" >:: ae ~skip:false "Hello, World!" (fun _ -> hello);
]

let () =
  run_test_tt_main ("Hello World tests" >::: tests)
