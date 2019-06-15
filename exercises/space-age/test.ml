open Base
open OUnit2
open Space_age
let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip ~delta:delta exp got _ctxt =
  sk skip;
  let result = got () in
  let msg = Printf.sprintf "Expected %f got %f, difference is greater than %f"
                    exp result delta in
  assert_bool msg (cmp_float ~epsilon:delta exp result)

let tests = [
   "age on Earth" >::
      ae ~skip:true ~delta:0.005 31.69 (fun _ -> age_on Earth 1000000000);
   "age on Mercury" >::
      ae ~skip:true ~delta:0.005 280.88 (fun _ -> age_on Mercury 2134835688);
   "age on Venus" >::
      ae ~skip:true ~delta:0.005 9.78 (fun _ -> age_on Venus 189839836);
   "age on Mars" >::
      ae ~skip:true ~delta:0.005 39.25 (fun _ -> age_on Mars 2329871239);
   "age on Jupiter" >::
      ae ~skip:true ~delta:0.005 2.41 (fun _ -> age_on Jupiter 901876382);
   "age on Saturn" >::
      ae ~skip:true ~delta:0.005 3.23 (fun _ -> age_on Saturn 3000000000);
   "age on Uranus" >::
      ae ~skip:true ~delta:0.005 1.21 (fun _ -> age_on Uranus 3210123456);
   "age on Neptune" >::
      ae ~skip:true ~delta:0.005 1.58 (fun _ -> age_on Neptune 8210123456);
]

let () =
  run_test_tt_main ("space-age tests" >::: tests)
