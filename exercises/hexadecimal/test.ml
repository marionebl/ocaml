open OUnit2
open Hexadecimal

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
  sk skip;
  assert_equal exp (got ())

let tests = ["1 to 1"    >:: ae ~skip:true 1 (fun _ -> to_int  "1");
             "c to 12"   >:: ae ~skip:true 12 (fun _ -> to_int  "c");
             "10 to 16"  >:: ae ~skip:true 16 (fun _ -> to_int  "10");
             "af to 175" >:: ae ~skip:true 175 (fun _ -> to_int  "af");
             "100 to 256" >:: ae ~skip:true 256 (fun _ -> to_int  "100");
             "19ace to 105166" >:: ae ~skip:true 105166 (fun _ -> to_int  "19ace");
             "carrot to 0" >:: ae ~skip:true 0 (fun _ -> to_int  "carrot");
             "000000 to 0" >:: ae ~skip:true 0 (fun _ -> to_int  "000000");
             "ffffff to 16777215" >:: ae ~skip:true 16777215 (fun _ -> to_int  "ffffff");
             "ffff00 to 16776960" >:: ae ~skip:true 16776960 (fun _ -> to_int  "ffff00");
            ]

let () =
  run_test_tt_main ("hexidecimal tests" >::: tests)
