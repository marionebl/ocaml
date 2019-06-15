open OUnit2
open Etl

let sk cond =
  let skippable = try String.equal (Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt =
  sk skip;
  let printer xs = String.concat ";" (List.map (fun (ch, n) -> Printf.sprintf "(%c,%d)" ch n) xs) in
  assert_equal exp (got ()) ~printer

let tests = [
   "a single letter" >::
      ae ~skip:true [('a', 1)] 
         (fun _ -> transform [(1, ['A'])]);
   "single score with multiple letters" >::
      ae ~skip:true [('a', 1); ('e', 1); ('i', 1); ('o', 1); ('u', 1)] 
         (fun _ -> transform [(1, ['A'; 'E'; 'I'; 'O'; 'U'])]);
   "multiple scores with multiple letters" >::
      ae ~skip:true [('a', 1); ('d', 2); ('e', 1); ('g', 2)] 
         (fun _ -> transform [(1, ['A'; 'E']); (2, ['D'; 'G'])]);
   "multiple scores with differing numbers of letters" >::
      ae ~skip:true [('a', 1); ('b', 3); ('c', 3); ('d', 2); ('e', 1); 
          ('f', 4); ('g', 2); ('h', 4); ('i', 1); ('j', 8); 
          ('k', 5); ('l', 1); ('m', 3); ('n', 1); ('o', 1); 
          ('p', 3); ('q', 10); ('r', 1); ('s', 1); ('t', 1); 
          ('u', 1); ('v', 4); ('w', 4); ('x', 8); ('y', 4); 
          ('z', 10)] 
         (fun _ -> transform [(1, ['A'; 'E'; 'I'; 'O'; 'U'; 'L'; 'N'; 'R'; 'S'; 'T']); 
                     (2, ['D'; 'G']); 
                     (3, ['B'; 'C'; 'M'; 'P']); 
                     (4, ['F'; 'H'; 'V'; 'W'; 'Y']); 
                     (5, ['K']); 
                     (8, ['J'; 'X']); 
                     (10, ['Q'; 'Z'])]);
]

let () =
  run_test_tt_main ("etl tests" >::: tests)
