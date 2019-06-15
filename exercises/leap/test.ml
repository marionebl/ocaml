open OUnit2
open Leap

let sk cond =
  let skippable = try String.equal (Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
  sk skip;
  assert_equal exp (got ()) ~printer:string_of_bool

let tests = [
  "year not divisible by 4: common year" >::
    ae ~skip:false false (fun _ -> leap_year 2015);
  "year divisible by 4, not divisible by 100: leap year" >::
    ae ~skip:false true (fun _ -> leap_year 1996);
  "year divisible by 100, not divisible by 400: common year" >::
    ae ~skip:false false (fun _ -> leap_year 2100);
  "year divisible by 400: leap year" >::
    ae ~skip:false true (fun _ -> leap_year 2000);
  "year divisible by 200, not divisible by 400: common year" >::
    ae ~skip:false false (fun _ -> leap_year 1800);
]

let () =
  run_test_tt_main ("leap tests" >::: tests)
