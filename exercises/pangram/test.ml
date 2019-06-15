open OUnit2
open Pangram
let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
   sk skip;
   assert_equal ~printer:string_of_bool exp (got ())

let tests = [
   "sentence empty" >::
      ae ~skip:true false (fun _ -> is_pangram "");
   "recognizes a perfect lower case pangram" >::
      ae ~skip:true true (fun _ -> is_pangram "abcdefghijklmnopqrstuvwxyz");
   "pangram with only lower case" >::
      ae ~skip:true true (fun _ -> is_pangram "the quick brown fox jumps over the lazy dog");
   "missing character 'x'" >::
      ae ~skip:true false (fun _ -> is_pangram "a quick movement of the enemy will jeopardize five gunboats");
   "another missing character, e.g. 'h'" >::
      ae ~skip:true false (fun _ -> is_pangram "five boxing wizards jump quickly at it");
   "pangram with underscores" >::
      ae ~skip:true true (fun _ -> is_pangram "the_quick_brown_fox_jumps_over_the_lazy_dog");
   "pangram with numbers" >::
      ae ~skip:true true (fun _ -> is_pangram "the 1 quick brown fox jumps over the 2 lazy dogs");
   "missing letters replaced by numbers" >::
      ae ~skip:true false (fun _ -> is_pangram "7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog");
   "pangram with mixed case and punctuation" >::
      ae ~skip:true true (fun _ -> is_pangram "\"Five quacking Zephyrs jolt my wax bed.\"");
   "upper and lower case versions of the same character should not be counted separately" >::
      ae ~skip:true false (fun _ -> is_pangram "the quick brown fox jumps over with lazy FX");
]

let () =
  run_test_tt_main ("pangram tests" >::: tests)
