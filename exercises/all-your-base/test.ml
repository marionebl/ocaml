open OUnit2
open All_your_base

let option_printer = function
  | None -> "None"
  | Some xs -> "Some [" ^ String.concat ";" (List.map string_of_int xs) ^ "]"

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ?(skip=false) exp got _test_ctxt =
  sk skip;
  assert_equal exp (got ()) ~printer:option_printer

let tests = [
  "single bit one to decimal" >::
    ae ~skip:true (Some [1]) (fun _ -> convert_bases ~from:2 ~digits:[1] ~target:10);
  "binary to single decimal" >::
    ae ~skip:true (Some [5]) (fun _ -> convert_bases ~from:2 ~digits:[1; 0; 1] ~target:10);
  "single decimal to binary" >::
    ae ~skip:true (Some [1; 0; 1]) (fun _ -> convert_bases ~from:10 ~digits:[5] ~target:2);
  "binary to multiple decimal" >::
    ae ~skip:true (Some [4; 2]) (fun _ -> convert_bases ~from:2 ~digits:[1; 0; 1; 0; 1; 0] ~target:10);
  "decimal to binary" >::
    ae ~skip:true (Some [1; 0; 1; 0; 1; 0]) (fun _ -> convert_bases ~from:10 ~digits:[4; 2] ~target:2);
  "trinary to hexadecimal" >::
    ae ~skip:true (Some [2; 10]) (fun _ -> convert_bases ~from:3 ~digits:[1; 1; 2; 0] ~target:16);
  "hexadecimal to trinary" >::
    ae ~skip:true (Some [1; 1; 2; 0]) (fun _ -> convert_bases ~from:16 ~digits:[2; 10] ~target:3);
  "15-bit integer" >::
    ae ~skip:true (Some [6; 10; 45]) (fun _ -> convert_bases ~from:97 ~digits:[3; 46; 60] ~target:73);
  "empty list" >::
    ae ~skip:true (Some [0]) (fun _ -> convert_bases ~from:2 ~digits:[] ~target:10);
  "single zero" >::
    ae ~skip:true (Some [0]) (fun _ -> convert_bases ~from:10 ~digits:[0] ~target:2);
  "multiple zeros" >::
    ae ~skip:true (Some [0]) (fun _ -> convert_bases ~from:10 ~digits:[0; 0; 0] ~target:2);
  "leading zeros" >::
    ae ~skip:true (Some [4; 2]) (fun _ -> convert_bases ~from:7 ~digits:[0; 6; 0] ~target:10);
  "input base is one" >::
    ae ~skip:true None (fun _ -> convert_bases ~from:1 ~digits:[0] ~target:10);
  "input base is zero" >::
    ae ~skip:true None (fun _ -> convert_bases ~from:0 ~digits:[] ~target:10);
  "input base is negative" >::
    ae ~skip:true None (fun _ -> convert_bases ~from:(-2) ~digits:[1] ~target:10);
  "negative digit" >::
    ae ~skip:true None (fun _ -> convert_bases ~from:2 ~digits:[1; -1; 1; 0; 1; 0] ~target:10);
  "invalid positive digit" >::
    ae ~skip:true None (fun _ -> convert_bases ~from:2 ~digits:[1; 2; 1; 0; 1; 0] ~target:10);
  "output base is one" >::
    ae ~skip:true None (fun _ -> convert_bases ~from:2 ~digits:[1; 0; 1; 0; 1; 0] ~target:1);
  "output base is zero" >::
    ae ~skip:true None (fun _ -> convert_bases ~from:10 ~digits:[7] ~target:0);
  "output base is negative" >::
    ae ~skip:true None (fun _ -> convert_bases ~from:2 ~digits:[1] ~target:(-7));
  "both bases are negative" >::
    ae ~skip:true None (fun _ -> convert_bases ~from:(-2) ~digits:[1] ~target:(-7));
]

let () =
  run_test_tt_main ("all-your-bases tests" >::: tests)
