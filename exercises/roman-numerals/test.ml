open OUnit2
open Roman_numerals
let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip expected actual _ctx = 
   sk skip;
   assert_equal ~printer:(fun x -> x) expected (actual ())

let tests = [
   "1 is a single I" >::
      ae ~skip:true "I" (fun _ -> to_roman 1);
   "2 is two I's" >::
      ae ~skip:true "II" (fun _ -> to_roman 2);
   "3 is three I's" >::
      ae ~skip:true "III" (fun _ -> to_roman 3);
   "4, being 5 - 1, is IV" >::
      ae ~skip:true "IV" (fun _ -> to_roman 4);
   "5 is a single V" >::
      ae ~skip:true "V" (fun _ -> to_roman 5);
   "6, being 5 + 1, is VI" >::
      ae ~skip:true "VI" (fun _ -> to_roman 6);
   "9, being 10 - 1, is IX" >::
      ae ~skip:true "IX" (fun _ -> to_roman 9);
   "20 is two X's" >::
      ae ~skip:true "XXVII" (fun _ -> to_roman 27);
   "48 is not 50 - 2 but rather 40 + 8" >::
      ae ~skip:true "XLVIII" (fun _ -> to_roman 48);
   "49 is not 40 + 5 + 4 but rather 50 - 10 + 10 - 1" >::
      ae ~skip:true "XLIX" (fun _ -> to_roman 49);
   "50 is a single L" >::
      ae ~skip:true "LIX" (fun _ -> to_roman 59);
   "90, being 100 - 10, is XC" >::
      ae ~skip:true "XCIII" (fun _ -> to_roman 93);
   "100 is a single C" >::
      ae ~skip:true "CXLI" (fun _ -> to_roman 141);
   "60, being 50 + 10, is LX" >::
      ae ~skip:true "CLXIII" (fun _ -> to_roman 163);
   "400, being 500 - 100, is CD" >::
      ae ~skip:true "CDII" (fun _ -> to_roman 402);
   "500 is a single D" >::
      ae ~skip:true "DLXXV" (fun _ -> to_roman 575);
   "900, being 1000 - 100, is CM" >::
      ae ~skip:true "CMXI" (fun _ -> to_roman 911);
   "1000 is a single M" >::
      ae ~skip:true "MXXIV" (fun _ -> to_roman 1024);
   "3000 is three M's" >::
      ae ~skip:true "MMM" (fun _ -> to_roman 3000);
]

let () =
    run_test_tt_main ("roman-numerals test" >::: tests) 
