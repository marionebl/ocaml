open Base
open OUnit2
open Anagram

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ?(skip=false) exp got _test_ctxt =
  let printer = String.concat ~sep:";" in
  sk skip;
  assert_equal exp (got ()) ~printer

let tests = [
  "no matches" >::
    ae ~skip:true [] (fun _ -> anagrams "diaper" ["hello"; "world"; "zombies"; "pants"]);
  "detects two anagrams" >::
    ae ~skip:true ["stream"; "maters"] (fun _ -> anagrams "master" ["stream"; "pigeon"; "maters"]);
  "does not detect anagram subsets" >::
    ae ~skip:true [] (fun _ -> anagrams "good" ["dog"; "goody"]);
  "detects anagram" >::
    ae ~skip:true ["inlets"] (fun _ -> anagrams "listen" ["enlists"; "google"; "inlets"; "banana"]);
  "detects three anagrams" >::
    ae ~skip:true ["gallery"; "regally"; "largely"] (fun _ -> anagrams "allergy" ["gallery"; "ballerina"; "regally"; "clergy"; "largely"; "leading"]);
  "does not detect non-anagrams with identical checksum" >::
    ae ~skip:true [] (fun _ -> anagrams "mass" ["last"]);
  "detects anagrams case-insensitively" >::
    ae ~skip:true ["Carthorse"] (fun _ -> anagrams "Orchestra" ["cashregister"; "Carthorse"; "radishes"]);
  "detects anagrams using case-insensitive subject" >::
    ae ~skip:true ["carthorse"] (fun _ -> anagrams "Orchestra" ["cashregister"; "carthorse"; "radishes"]);
  "detects anagrams using case-insensitive possible matches" >::
    ae ~skip:true ["Carthorse"] (fun _ -> anagrams "orchestra" ["cashregister"; "Carthorse"; "radishes"]);
  "does not detect a anagram if the original word is repeated" >::
    ae ~skip:true [] (fun _ -> anagrams "go" ["go Go GO"]);
  "anagrams must use all letters exactly once" >::
    ae ~skip:true [] (fun _ -> anagrams "tapper" ["patter"]);
  "words are not anagrams of themselves (case-insensitive)" >::
    ae ~skip:true [] (fun _ -> anagrams "BANANA" ["BANANA"; "Banana"; "banana"]);
]

let () =
  run_test_tt_main ("anagrams tests" >::: tests)
