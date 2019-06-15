open OUnit2
open Rectangles

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
   sk skip;
   assert_equal exp (got ()) ~printer:string_of_int

let tests = [
   "no rows" >::
      ae ~skip:true 0 (fun _ -> count_rectangles  [||]);
   "no columns" >::
      ae ~skip:true 0 (fun _ -> count_rectangles  [|""|]);
   "no rectangles" >::
      ae ~skip:true 0 (fun _ -> count_rectangles  [|" "|]);
   "one rectangle" >::
      ae ~skip:true 1 (fun _ -> count_rectangles  [|"+-+"; 
                               "| |"; 
                               "+-+"|]);
   "two rectangles without shared parts" >::
      ae ~skip:true 2 (fun _ -> count_rectangles  [|"  +-+"; 
                               "  | |"; 
                               "+-+-+"; 
                               "| |  "; 
                               "+-+  "|]);
   "five rectangles with shared parts" >::
      ae ~skip:true 5 (fun _ -> count_rectangles  [|"  +-+"; 
                               "  | |"; 
                               "+-+-+"; 
                               "| | |"; 
                               "+-+-+"|]);
   "rectangle of height 1 is counted" >::
      ae ~skip:true 1 (fun _ -> count_rectangles  [|"+--+"; 
                               "+--+"|]);
   "rectangle of width 1 is counted" >::
      ae ~skip:true 1 (fun _ -> count_rectangles  [|"++"; 
                               "||"; 
                               "++"|]);
   "1x1 square is counted" >::
      ae ~skip:true 1 (fun _ -> count_rectangles  [|"++";
                               "++"|]);
   "only complete rectangles are counted" >::
      ae ~skip:true 1 (fun _ -> count_rectangles  [|"  +-+"; 
                               "    |"; 
                               "+-+-+"; 
                               "| | -"; 
                               "+-+-+"|]);
   "rectangles can be of different sizes" >::
      ae ~skip:true 3 (fun _ -> count_rectangles  [|"+------+----+"; 
                               "|      |    |"; 
                               "+---+--+    |"; 
                               "|   |       |"; 
                               "+---+-------+"|]);
   "corner is required for a rectangle to be complete" >::
      ae ~skip:true 2 (fun _ -> count_rectangles  [|"+------+----+"; 
                               "|      |    |"; 
                               "+------+    |"; 
                               "|   |       |"; 
                               "+---+-------+"|]);
   "large input with many rectangles" >::
      ae ~skip:true 60 (fun _ -> count_rectangles  [|"+---+--+----+"; 
                                "|   +--+----+"; 
                                "+---+--+    |"; 
                                "|   +--+----+"; 
                                "+---+--+--+-+"; 
                                "+---+--+--+-+"; 
                                "+------+  | |"; 
                                "          +-+"|]);
]

let () =
  run_test_tt_main ("rectangles tests" >::: tests)
