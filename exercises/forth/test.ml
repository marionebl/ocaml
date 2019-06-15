open Base
open OUnit2
open Forth

let print_int_list_option (xs: int list option) = match xs with
| None -> "None"
| Some xs -> "Some [" ^ String.concat ~sep:";" (List.map ~f:Int.to_string xs) ^ "]"

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
  sk skip;
  assert_equal ~printer:print_int_list_option exp (got ())

let parsing_and_numbers_tests = [
   "numbers just get pushed onto the stack" >::
     ae ~skip:true (Some [1; 2; 3; 4; 5]) (fun _ -> evaluate ["1 2 3 4 5"]);
]


let addition_tests = [
   "can add two numbers" >::
     ae ~skip:true (Some [3]) (fun _ -> evaluate ["1 2 +"]);
   "errors if there is nothing on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["+"]);
   "errors if there is only one value on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["1 +"]);
]


let subtraction_tests = [
   "can subtract two numbers" >::
     ae ~skip:true (Some [-1]) (fun _ -> evaluate ["3 4 -"]);
   "errors if there is nothing on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["-"]);
   "errors if there is only one value on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["1 -"]);
]


let multiplication_tests = [
   "can multiply two numbers" >::
     ae ~skip:true (Some [8]) (fun _ -> evaluate ["2 4 *"]);
   "errors if there is nothing on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["*"]);
   "errors if there is only one value on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["1 *"]);
]


let division_tests = [
   "can divide two numbers" >::
     ae ~skip:true (Some [4]) (fun _ -> evaluate ["12 3 /"]);
   "performs integer division" >::
     ae ~skip:true (Some [2]) (fun _ -> evaluate ["8 3 /"]);
   "errors if dividing by zero" >::
     ae ~skip:true None (fun _ -> evaluate ["4 0 /"]);
   "errors if there is nothing on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["/"]);
   "errors if there is only one value on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["1 /"]);
]


let combined_arithmetic_tests = [
   "addition and subtraction" >::
     ae ~skip:true (Some [-1]) (fun _ -> evaluate ["1 2 + 4 -"]);
   "multiplication and division" >::
     ae ~skip:true (Some [2]) (fun _ -> evaluate ["2 4 * 3 /"]);
]


let dup_tests = [
   "copies the top value on the stack" >::
     ae ~skip:true (Some [1; 1]) (fun _ -> evaluate ["1 DUP"]);
   "is case-insensitive" >::
     ae ~skip:true (Some [1; 2; 2]) (fun _ -> evaluate ["1 2 Dup"]);
   "errors if there is nothing on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["dup"]);
]


let drop_tests = [
   "removes the top value on the stack if it is the only one" >::
     ae ~skip:true (Some []) (fun _ -> evaluate ["1 drop"]);
   "removes the top value on the stack if it is not the only one" >::
     ae ~skip:true (Some [1]) (fun _ -> evaluate ["1 2 drop"]);
   "errors if there is nothing on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["drop"]);
]


let swap_tests = [
   "swaps the top two values on the stack if they are the only ones" >::
     ae ~skip:true (Some [2; 1]) (fun _ -> evaluate ["1 2 swap"]);
   "swaps the top two values on the stack if they are not the only ones" >::
     ae ~skip:true (Some [1; 3; 2]) (fun _ -> evaluate ["1 2 3 swap"]);
   "errors if there is nothing on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["swap"]);
   "errors if there is only one value on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["1 swap"]);
]


let over_tests = [
   "copies the second element if there are only two" >::
     ae ~skip:true (Some [1; 2; 1]) (fun _ -> evaluate ["1 2 over"]);
   "copies the second element if there are more than two" >::
     ae ~skip:true (Some [1; 2; 3; 2]) (fun _ -> evaluate ["1 2 3 over"]);
   "errors if there is nothing on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["over"]);
   "errors if there is only one value on the stack" >::
     ae ~skip:true None (fun _ -> evaluate ["1 over"]);
]


let user_defined_words_tests = [
   "can consist of built-in words" >::
     ae ~skip:true (Some [1; 1; 1]) (fun _ -> evaluate [": dup-twice dup dup ;"; "1 dup-twice"]);
   "execute in the right order" >::
     ae ~skip:true (Some [1; 2; 3]) (fun _ -> evaluate [": countup 1 2 3 ;"; "countup"]);
   "can override other user-defined words" >::
     ae ~skip:true (Some [1; 1; 1]) (fun _ -> evaluate [": foo dup ;"; ": foo dup dup ;"; "1 foo"]);
   "can override built-in words" >::
     ae ~skip:true (Some [1; 1]) (fun _ -> evaluate [": swap dup ;"; "1 swap"]);
   "can override built-in operators" >::
     ae ~skip:true (Some [12]) (fun _ -> evaluate [": + * ;"; "3 4 +"]);
   "cannot redefine numbers" >::
     ae ~skip:true None (fun _ -> evaluate [": 1 2 ;"]);
   "errors if executing a non-existent word" >::
     ae ~skip:true None (fun _ -> evaluate ["foo"]);
]

let () =
  run_test_tt_main (
    "forth tests" >:::
      List.concat [
        parsing_and_numbers_tests; 
        addition_tests; 
        subtraction_tests; 
        multiplication_tests; 
        division_tests;
        combined_arithmetic_tests; 
        dup_tests; 
        drop_tests;
        swap_tests; 
        over_tests; 
        user_defined_words_tests
        ]
  )
