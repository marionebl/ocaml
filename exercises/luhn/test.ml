open Base
open OUnit2
open Luhn

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let assert_valid ~skip expected input _test_ctxt = 
  sk skip;
  assert_equal ~printer:Bool.to_string expected (valid input)

let tests = [
  "single digit strings can not be valid" >::
    assert_valid ~skip:true false "1";
  "a single zero is invalid" >::
    assert_valid ~skip:true false "0";
  "a simple valid SIN that remains valid if reversed" >::
    assert_valid ~skip:true true "059";
  "a simple valid SIN that becomes invalid if reversed" >::
    assert_valid ~skip:true true "59";
  "a valid Canadian SIN" >::
    assert_valid ~skip:true true "055 444 285";
  "invalid Canadian SIN" >::
    assert_valid ~skip:true false "055 444 286";
  "invalid credit card" >::
    assert_valid ~skip:true false "8273 1232 7352 0569";
  "valid strings with a non-digit included become invalid" >::
    assert_valid ~skip:true false "055a 444 285";
  "valid strings with punctuation included become invalid" >::
    assert_valid ~skip:true false "055-444-285";
  "valid strings with symbols included become invalid" >::
    assert_valid ~skip:true false "055\194\163 444$ 285";
  "single zero with space is invalid" >::
    assert_valid ~skip:true false " 0";
  "more than a single zero is valid" >::
    assert_valid ~skip:true true "0000 0";
  "input digit 9 is correctly converted to output digit 9" >::
    assert_valid ~skip:true true "091";
  "strings with non-digits is invalid" >::
    assert_valid ~skip:true false ":9";
]

let () =
  run_test_tt_main ("luhn tests" >::: tests)
