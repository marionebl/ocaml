open Base
open OUnit2
open Run_length_encoding
let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
  sk skip;
  assert_equal exp (got ()) ~printer:Fn.id

let encode_tests = [
   "empty string" >::
     ae ~skip:true "" (fun _ -> encode "");
   "single characters only are encoded without count" >::
     ae ~skip:true "XYZ" (fun _ -> encode "XYZ");
   "string with no single characters" >::
     ae ~skip:true "2A3B4C" (fun _ -> encode "AABBBCCCC");
   "single characters mixed with repeated characters" >::
     ae ~skip:true "12WB12W3B24WB" (fun _ -> encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB");
   "multiple whitespace mixed in string" >::
     ae ~skip:true "2 hs2q q2w2 " (fun _ -> encode "  hsqq qww  ");
   "lowercase characters" >::
     ae ~skip:true "2a3b4c" (fun _ -> encode "aabbbcccc");
]


let decode_tests = [
   "empty string" >::
     ae ~skip:true "" (fun _ -> decode "");
   "single characters only" >::
     ae ~skip:true "XYZ" (fun _ -> decode "XYZ");
   "string with no single characters" >::
     ae ~skip:true "AABBBCCCC" (fun _ -> decode "2A3B4C");
   "single characters with repeated characters" >::
     ae ~skip:true "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB" (fun _ -> decode "12WB12W3B24WB");
   "multiple whitespace mixed in string" >::
     ae ~skip:true "  hsqq qww  " (fun _ -> decode "2 hs2q q2w2 ");
   "lower case string" >::
     ae ~skip:true "aabbbcccc" (fun _ -> decode "2a3b4c");
]


let encode_and_then_decode_tests = [
   "encode followed by decode gives original string" >::
     ae ~skip:true "zzz ZZ  zZ" (fun _ -> encode "zzz ZZ  zZ" |> decode);
]

let () =
  run_test_tt_main (
    "run length encoding tests" >:::
      List.concat [encode_tests; decode_tests; encode_and_then_decode_tests]
  )
