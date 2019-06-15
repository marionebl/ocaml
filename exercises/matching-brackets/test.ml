open Base
open OUnit2
open Matching_brackets
let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt =
  sk skip;
  assert_equal exp (got ()) ~printer:Bool.to_string

let tests = [
  "paired square brackets" >::
    ae ~skip:true true (fun _ -> are_balanced "[]");
  "empty string" >::
    ae ~skip:true true (fun _ -> are_balanced "");
  "unpaired brackets" >::
    ae ~skip:true false (fun _ -> are_balanced "[[");
  "wrong ordered brackets" >::
    ae ~skip:true false (fun _ -> are_balanced "}{");
  "wrong closing bracket" >::
    ae ~skip:true false (fun _ -> are_balanced "{]");
  "paired with whitespace" >::
    ae ~skip:true true (fun _ -> are_balanced "{ }");
  "partially paired brackets" >::
    ae ~skip:true false (fun _ -> are_balanced "{[])");
  "simple nested brackets" >::
    ae ~skip:true true (fun _ -> are_balanced "{[]}");
  "several paired brackets" >::
    ae ~skip:true true (fun _ -> are_balanced "{}[]");
  "paired and nested brackets" >::
    ae ~skip:true true (fun _ -> are_balanced "([{}({}[])])");
  "unopened closing brackets" >::
    ae ~skip:true false (fun _ -> are_balanced "{[)][]}");
  "unpaired and nested brackets" >::
    ae ~skip:true false (fun _ -> are_balanced "([{])");
  "paired and wrong nested brackets" >::
    ae ~skip:true false (fun _ -> are_balanced "[({]})");
  "math expression" >::
    ae ~skip:true true (fun _ -> are_balanced "(((185 + 223.85) * 15) - 543)/2");
  "complex latex expression" >::
    ae ~skip:true true (fun _ -> are_balanced "\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)");
]

let () =
  run_test_tt_main ("matching-brackets tests" >::: tests)
