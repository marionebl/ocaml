open Base
open OUnit2
open Phone_number

let option_to_string f = function
  | None   -> "None"
  | Some x -> "Some " ^ f x
let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt =
  sk skip;
  assert_equal ~printer:(option_to_string Fn.id) exp (got ())

let tests = [
  "cleans the number" >::
    ae ~skip:true (Some "2234567890") (fun _ -> number "(223) 456-7890");
  "cleans numbers with dots" >::
    ae ~skip:true (Some "2234567890") (fun _ -> number "223.456.7890");
  "cleans numbers with multiple spaces" >::
    ae ~skip:true (Some "2234567890") (fun _ -> number "223 456   7890   ");
  "invalid when 9 digits" >::
    ae ~skip:true None (fun _ -> number "123456789");
  "invalid when 11 digits does not start with a 1" >::
    ae ~skip:true None (fun _ -> number "22234567890");
  "valid when 11 digits and starting with 1" >::
    ae ~skip:true (Some "2234567890") (fun _ -> number "12234567890");
  "valid when 11 digits and starting with 1 even with punctuation" >::
    ae ~skip:true (Some "2234567890") (fun _ -> number "+1 (223) 456-7890");
  "invalid when more than 11 digits" >::
    ae ~skip:true None (fun _ -> number "321234567890");
  "invalid with letters" >::
    ae ~skip:true None (fun _ -> number "123-abc-7890");
  "invalid with punctuations" >::
    ae ~skip:true None (fun _ -> number "123-@:!-7890");
  "invalid if area code starts with 0" >::
    ae ~skip:true None (fun _ -> number "(023) 456-7890");
  "invalid if area code starts with 1" >::
    ae ~skip:true None (fun _ -> number "(123) 456-7890");
  "invalid if exchange code starts with 0" >::
    ae ~skip:true None (fun _ -> number "(223) 056-7890");
  "invalid if exchange code starts with 1" >::
    ae ~skip:true None (fun _ -> number "(223) 156-7890");
]

let () =
  run_test_tt_main ("phone-number tests" >::: tests)
