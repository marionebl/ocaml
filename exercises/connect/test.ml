open OUnit2
open Connect

let show_player = function
| Some X -> "X"
| Some O -> "O"
| None -> "None"

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ?(skip=false) exp got = 
  sk skip;
  assert_equal ~printer:show_player exp (got ())

let tests = [
  "an empty board has no winner" >::(fun _ctxt ->
    let board = [
      ". . . . ."; 
      " . . . . ."; 
      "  . . . . ."; 
      "   . . . . ."; 
      "    . . . . ."
      ] 
    in
    ae ~skip:true None (fun _ -> connect board)
  );
  "X can win on a 1x1 board" >::(fun _ctxt ->
    let board = [
      "X"
      ] 
    in
    ae ~skip:true (Some X) (fun _ -> connect board)
  );
  "O can win on a 1x1 board" >::(fun _ctxt ->
    let board = [
      "O"
      ] 
    in
    ae ~skip:true (Some O) (fun _ -> connect board)
  );
  "only edges does not make a winner" >::(fun _ctxt ->
    let board = [
      "O O O X"; 
      " X . . X"; 
      "  X . . X"; 
      "   X O O O"
      ] 
    in
    ae ~skip:true None (fun _ -> connect board)
  );
  "illegal diagonal does not make a winner" >::(fun _ctxt ->
    let board = [
      "X O . ."; 
      " O X X X"; 
      "  O X O ."; 
      "   . O X ."; 
      "    X X O O"
      ] 
    in
    ae ~skip:true None (fun _ -> connect board)
  );
  "nobody wins crossing adjacent angles" >::(fun _ctxt ->
    let board = [
      "X . . ."; 
      " . X O ."; 
      "  O . X O"; 
      "   . O . X"; 
      "    . . O ."
      ] 
    in
    ae ~skip:true None (fun _ -> connect board)
  );
  "X wins crossing from left to right" >::(fun _ctxt ->
    let board = [
      ". O . ."; 
      " O X X X"; 
      "  O X O ."; 
      "   X X O X"; 
      "    . O X ."
      ] 
    in
    ae ~skip:true (Some X) (fun _ -> connect board)
  );
  "O wins crossing from top to bottom" >::(fun _ctxt ->
    let board = [
      ". O . ."; 
      " O X X X"; 
      "  O O O ."; 
      "   X X O X"; 
      "    . O X ."
      ] 
    in
    ae ~skip:true (Some O) (fun _ -> connect board)
  );
  "X wins using a convoluted path" >::(fun _ctxt ->
    let board = [
      ". X X . ."; 
      " X . X . X"; 
      "  . X . X ."; 
      "   . X X . ."; 
      "    O O O O O"
      ] 
    in
    ae ~skip:true (Some X) (fun _ -> connect board)
  );
  "X wins using a spiral path" >::(fun _ctxt ->
    let board = [
      "O X X X X X X X X"; 
      " O X O O O O O O O"; 
      "  O X O X X X X X O"; 
      "   O X O X O O O X O"; 
      "    O X O X X X O X O"; 
      "     O X O O O X O X O"; 
      "      O X X X X X O X O"; 
      "       O O O O O O O X O"; 
      "        X X X X X X X X O"
      ] 
    in
    ae ~skip:true (Some X) (fun _ -> connect board)
  );
]

let () =
  run_test_tt_main ("connect tests" >::: tests)
