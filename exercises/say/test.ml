open Base
open OUnit2
open Say
let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _ctx = 
   sk skip;
   assert_equal ~printer:(Option.value ~default:"None") exp (got ())

let tests = [
   "zero" >:: (
      ae ~skip:true (Some "zero")
         (fun _ -> in_english 0L));
   "one" >:: (
      ae ~skip:true (Some "one")
         (fun _ -> in_english 1L));
   "fourteen" >:: (
      ae ~skip:true (Some "fourteen")
         (fun _ -> in_english 14L));
   "twenty" >:: (
      ae ~skip:true (Some "twenty")
         (fun _ -> in_english 20L));
   "twenty-two" >:: (
      ae ~skip:true (Some "twenty-two")
         (fun _ -> in_english 22L));
   "one hundred" >:: (
      ae ~skip:true (Some "one hundred")
         (fun _ -> in_english 100L));
   "one hundred twenty-three" >:: (
      ae ~skip:true (Some "one hundred twenty-three")
         (fun _ -> in_english 123L));
   "one thousand" >:: (
      ae ~skip:true (Some "one thousand")
         (fun _ -> in_english 1000L));
   "one thousand two hundred thirty-four" >:: (
      ae ~skip:true (Some "one thousand two hundred thirty-four")
         (fun _ -> in_english 1234L));
   "one million" >:: (
      ae ~skip:true (Some "one million")
         (fun _ -> in_english 1000000L));
   "one million two thousand three hundred forty-five" >:: (
      ae ~skip:true (Some "one million two thousand three hundred forty-five")
         (fun _ -> in_english 1002345L));
   "one billion" >:: (
      ae ~skip:true (Some "one billion")
         (fun _ -> in_english 1000000000L));
   "a big number" >:: (
      ae ~skip:true (Some "nine hundred eighty-seven billion six hundred fifty-four million three hundred twenty-one thousand one hundred twenty-three")
         (fun _ -> in_english 987654321123L));
   "numbers below zero are out of range" >:: (
      ae ~skip:true None
         (fun _ -> in_english (-1L)));
   "numbers above 999,999,999,999 are out of range" >:: (
      ae ~skip:true None
         (fun _ -> in_english 1000000000000L));
  "all numbers from 1 to 10_000 can be spelt">::(fun _ ->
      sk true;
      assert_bool "range test" (Sequence.range 0 10_000
                                |> Sequence.map ~f:(fun n -> Int64.of_int n |> in_english)
                                |> Sequence.filter ~f:(Option.is_none)
                                |> Sequence.is_empty));
]

let () =
  run_test_tt_main ("say tests" >::: tests)
