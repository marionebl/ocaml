open OUnit2
open Binary_search

let option_to_string f = function
  | None   -> "None"
  | Some x -> "Some " ^ f x

let sk cond =
  let skippable = try String.equal (Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ?(skip=false) exp got _test_ctxt =
  sk skip;
  assert_equal ~printer:(option_to_string string_of_int) exp (got ())

let tests = [
  "finds a value in an array with one element" >::
    ae ~skip:true (Some 0) (fun _ -> find [|6|] 6);
  "finds a value in the middle of an array" >::
    ae ~skip:true (Some 3) (fun _ -> find [|1; 3; 4; 6; 8; 9; 11|] 6);
  "finds a value at the beginning of an array" >::
    ae ~skip:true (Some 0) (fun _ -> find [|1; 3; 4; 6; 8; 9; 11|] 1);
  "finds a value at the end of an array" >::
    ae ~skip:true (Some 6) (fun _ -> find [|1; 3; 4; 6; 8; 9; 11|] 11);
  "finds a value in an array of odd length" >::
    ae ~skip:true (Some 9) (fun _ -> find [|1; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 634|] 144);
  "finds a value in an array of even length" >::
    ae ~skip:true (Some 5) (fun _ -> find [|1; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377|] 21);
  "identifies that a value is not included in the array" >::
    ae ~skip:true None (fun _ -> find [|1; 3; 4; 6; 8; 9; 11|] 7);
  "a value smaller than the array's smallest value is not included" >::
    ae ~skip:true None (fun _ -> find [|1; 3; 4; 6; 8; 9; 11|] 0);
  "a value larger than the array's largest value is not included" >::
    ae ~skip:true None (fun _ -> find [|1; 3; 4; 6; 8; 9; 11|] 13);
  "nothing is included in an empty array" >::
    ae ~skip:true None (fun _ -> find [||] 1);
]

let () =
  run_test_tt_main ("binary-search tests" >::: tests)
