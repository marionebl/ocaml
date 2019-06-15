open Base
open OUnit2
open Change

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let printer = Option.value_map ~default:"None" ~f:(fun xs -> String.concat ~sep:";" (List.map ~f:Int.to_string xs))

let ae ?(skip=false) exp got _test_ctxt = 
  sk skip;
  assert_equal ~printer exp (got ())

let tests = [
   "single coin change" >::
     ae ~skip:true (Some [25]) 
       (fun _ -> make_change ~target:25 ~coins:[1; 5; 10; 25; 100]);
   "multiple coin change" >::
     ae ~skip:true (Some [5; 10]) 
       (fun _ -> make_change ~target:15 ~coins:[1; 5; 10; 25; 100]);
   "change with Lilliputian Coins" >::
     ae ~skip:true (Some [4; 4; 15]) 
       (fun _ -> make_change ~target:23 ~coins:[1; 4; 15; 20; 50]);
   "change with Lower Elbonia Coins" >::
     ae ~skip:true (Some [21; 21; 21]) 
       (fun _ -> make_change ~target:63 ~coins:[1; 5; 10; 21; 25]);
   "large target values" >::
     ae ~skip:true (Some [2; 2; 5; 20; 20; 50; 100; 100; 100; 100; 100; 100; 100; 100; 100]) 
       (fun _ -> make_change ~target:999 ~coins:[1; 2; 5; 10; 20; 50; 100]);
   "possible change without unit coins available" >::
     ae ~skip:true (Some [2; 2; 2; 5; 10]) 
       (fun _ -> make_change ~target:21 ~coins:[2; 5; 10; 20; 50]);
   "another possible change without unit coins available" >::
     ae ~skip:true (Some [4; 4; 4; 5; 5; 5]) 
       (fun _ -> make_change ~target:27 ~coins:[4; 5]);
   "no coins make 0 change" >::
     ae ~skip:true (Some []) 
       (fun _ -> make_change ~target:0 ~coins:[1; 5; 10; 21; 25]);
   "error testing for change smaller than the smallest of coins" >::
     ae ~skip:true None 
       (fun _ -> make_change ~target:3 ~coins:[5; 10]);
   "error if no combination can add up to target" >::
     ae ~skip:true None 
       (fun _ -> make_change ~target:94 ~coins:[5; 10]);
   "cannot find negative change values" >::
     ae ~skip:true None 
       (fun _ -> make_change ~target:(-5) ~coins:[1; 2; 5]);
]

let () =
  run_test_tt_main ("change tests" >::: tests)
