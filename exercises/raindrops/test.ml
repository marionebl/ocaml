open OUnit2
open Raindrops
let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
   sk skip;
   assert_equal ~printer:(fun x -> x) exp (got ())

let tests = [
   "the sound for 1 is 1" >::
      ae ~skip:true "1" (fun _ -> raindrop  1);
   "the sound for 3 is Pling" >::
      ae ~skip:true "Pling" (fun _ -> raindrop  3);
   "the sound for 5 is Plang" >::
      ae ~skip:true "Plang" (fun _ -> raindrop  5);
   "the sound for 7 is Plong" >::
      ae ~skip:true "Plong" (fun _ -> raindrop  7);
   "the sound for 6 is Pling as it has a factor 3" >::
      ae ~skip:true "Pling" (fun _ -> raindrop  6);
   "2 to the power 3 does not make a raindrop sound as 3 is the exponent not the base" >::
      ae ~skip:true "8" (fun _ -> raindrop  8);
   "the sound for 9 is Pling as it has a factor 3" >::
      ae ~skip:true "Pling" (fun _ -> raindrop  9);
   "the sound for 10 is Plang as it has a factor 5" >::
      ae ~skip:true "Plang" (fun _ -> raindrop  10);
   "the sound for 14 is Plong as it has a factor of 7" >::
      ae ~skip:true "Plong" (fun _ -> raindrop  14);
   "the sound for 15 is PlingPlang as it has factors 3 and 5" >::
      ae ~skip:true "PlingPlang" (fun _ -> raindrop  15);
   "the sound for 21 is PlingPlong as it has factors 3 and 7" >::
      ae ~skip:true "PlingPlong" (fun _ -> raindrop  21);
   "the sound for 25 is Plang as it has a factor 5" >::
      ae ~skip:true "Plang" (fun _ -> raindrop  25);
   "the sound for 27 is Pling as it has a factor 3" >::
      ae ~skip:true "Pling" (fun _ -> raindrop  27);
   "the sound for 35 is PlangPlong as it has factors 5 and 7" >::
      ae ~skip:true "PlangPlong" (fun _ -> raindrop  35);
   "the sound for 49 is Plong as it has a factor 7" >::
      ae ~skip:true "Plong" (fun _ -> raindrop  49);
   "the sound for 52 is 52" >::
      ae ~skip:true "52" (fun _ -> raindrop  52);
   "the sound for 105 is PlingPlangPlong as it has factors 3, 5 and 7" >::
      ae ~skip:true "PlingPlangPlong" (fun _ -> raindrop  105);
   "the sound for 3125 is Plang as it has a factor 5" >::
      ae ~skip:true "Plang" (fun _ -> raindrop  3125);
]

let () =
  run_test_tt_main ("raindrops tests" >::: tests)
