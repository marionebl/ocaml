open OUnit2
open Difference_of_squares

let sk cond =
  let skippable = try String.equal (Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
  sk skip;
  assert_equal exp (got ())

let square_of_sum_tests = [
   "square of sum 1" >::
     ae ~skip:true 1 (fun _ -> square_of_sum 1);
   "square of sum 5" >::
     ae ~skip:true 225 (fun _ -> square_of_sum 5);
   "square of sum 100" >::
     ae ~skip:true 25502500 (fun _ -> square_of_sum 100);
]


let sum_of_squares_tests = [
   "sum of squares 1" >::
     ae ~skip:true 1 (fun _ -> sum_of_squares 1);
   "sum of squares 5" >::
     ae ~skip:true 55 (fun _ -> sum_of_squares 5);
   "sum of squares 100" >::
     ae ~skip:true 338350 (fun _ -> sum_of_squares 100);
]


let difference_of_squares_tests = [
   "difference of squares 1" >::
     ae ~skip:true 0 (fun _ -> difference_of_squares  1);
   "difference of squares 5" >::
     ae ~skip:true 170 (fun _ -> difference_of_squares  5);
   "difference of squares 100" >::
     ae ~skip:true 25164150 (fun _ -> difference_of_squares  100);
]

let () =
  run_test_tt_main (
    "difference of squares tests" >:::
      List.concat [square_of_sum_tests; sum_of_squares_tests; difference_of_squares_tests]
  )
