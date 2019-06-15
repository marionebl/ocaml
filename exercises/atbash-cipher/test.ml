open OUnit2
open Atbash_cipher

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ?(skip=false) exp got _test_ctxt = 
  sk skip;
  assert_equal ~printer:(fun x -> x) exp (got ())

let encode_tests = [
   "encode yes" >::
     ae ~skip:true "bvh" (fun _ -> encode "yes");
   "encode no" >::
     ae ~skip:true "ml" (fun _ -> encode "no");
   "encode OMG" >::
     ae ~skip:true "lnt" (fun _ -> encode "OMG");
   "encode spaces" >::
     ae ~skip:true "lnt" (fun _ -> encode "O M G");
   "encode mindblowingly" >::
     ae ~skip:true "nrmwy oldrm tob" (fun _ -> encode "mindblowingly");
   "encode numbers" >::
     ae ~skip:true "gvhgr mt123 gvhgr mt" (fun _ -> encode "Testing,1 2 3, testing.");
   "encode deep thought" >::
     ae ~skip:true "gifgs rhurx grlm" (fun _ -> encode "Truth is fiction.");
   "encode all the letters" >::
     ae ~skip:true "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt" (fun _ -> encode "The quick brown fox jumps over the lazy dog.");
]


let decode_tests = [
   "decode exercism" >::
     ae ~skip:true "exercism" (fun _ -> decode "vcvix rhn");
   "decode a sentence" >::
     ae ~skip:true "anobstacleisoftenasteppingstone" (fun _ -> decode "zmlyh gzxov rhlug vmzhg vkkrm thglm v");
   "decode numbers" >::
     ae ~skip:true "testing123testing" (fun _ -> decode "gvhgr mt123 gvhgr mt");
   "decode all the letters" >::
     ae ~skip:true "thequickbrownfoxjumpsoverthelazydog" (fun _ -> decode "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt");
   "decode with too many spaces" >::
     ae ~skip:true "exercism" (fun _ -> decode "vc vix    r hn");
   "decode with no spaces" >::
     ae ~skip:true "anobstacleisoftenasteppingstone" (fun _ -> decode "zmlyhgzxovrhlugvmzhgvkkrmthglmv");
]

let different_block_size_test = [
  "encode mindblowingly with a different block size" >::
    ae ~skip:true "n r m w y o l d r m t o b" (fun _ -> encode ~block_size:1 "mindblowingly");
]

let () =
  run_test_tt_main (
    "atbash-cipher tests" >:::
      List.concat [encode_tests; decode_tests; different_block_size_test]
  )
