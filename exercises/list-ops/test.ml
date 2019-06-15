open Base
open OUnit2

module L = List_ops

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let aei ~skip exp got _test_ctxt = 
  sk skip;
  assert_equal ~printer:Int.to_string exp (got ())

let ael ~skip exp got _test_ctxt =
  sk skip;
  assert_equal exp (got ()) ~printer:(fun xs -> String.concat ~sep:";" (List.map ~f:Int.to_string xs)) 

let is_odd n = n % 2 = 1

let tests = [
  "length of empty list">::
     aei ~skip:true 0 (fun _ -> L.length []);
   "length of normal list">::
     aei ~skip:true 4 (fun _ -> L.length [1;3;5;7]);
   "length of huge list">::
     aei ~skip:true 1_000_000 (fun _ -> L.length (List.range 0 1_000_000));
   "reverse of empty list">::
     ael ~skip:true [] (fun _ -> L.reverse []);
   "reverse of normal list">::
     ael ~skip:true [7;5;3;1] (fun _ -> L.reverse [1;3;5;7]);
   "reverse of huge list">::
     ael ~skip:true (List.range ~start:`exclusive ~stop:`inclusive ~stride:(-1) 1_000_000 0) 
         (fun _ -> L.reverse (List.range 0 1_000_000));
   "map of empty list">::
     ael ~skip:true [] (fun _ -> L.map ~f:((+) 1) []);
   "map of normal list">::
     ael ~skip:true [2;4;6;8] (fun _ -> L.map ~f:((+) 1) [1;3;5;7]);
   "map of huge list">::
     ael ~skip:true (List.range 1 1_000_001) (fun _ -> L.map ~f:((+) 1) (List.range 0 1_000_000));
   "filter of empty list">::
     ael ~skip:true [] (fun _ -> L.filter ~f:is_odd []);
   "filter of normal list">::
     ael ~skip:true [1;3] (fun _ -> L.filter ~f:is_odd [1;2;3;4]);
   "filter of huge list">::
     ael ~skip:true (List.range ~stride:2 1 1_000_000) 
         (fun _ -> L.filter ~f:is_odd (List.range 0 1_000_000));
   "fold of empty list">::
     aei ~skip:true 0 (fun _ -> L.fold ~init:0 ~f:(+) []);
   "fold of normal list">::
     aei ~skip:true 7 (fun _ -> L.fold ~init:(-3) ~f:(+) [1;2;3;4]);
   "fold of huge list">::
     aei ~skip:true (List.fold ~init:0 ~f:(+) (List.range 0 1_000_000)) 
         (fun _ -> L.fold ~init:0 ~f:(+) (List.range 0 1_000_000));
   "append of empty lists">::
     ael ~skip:true [] (fun _ -> L.append [] []);
   "append of empty and non-empty list">::
     ael ~skip:true [1;2;3;4] (fun _ -> L.append [] [1;2;3;4]);
   "append of non-empty and empty list">::
     ael ~skip:true [1;2;3;4] (fun _ -> L.append [1;2;3;4] []);
   "append of non-empty lists">::
   ael ~skip:true [1;2;3;4;5] (fun _ -> L.append [1;2;3] [4;5]);
   "append of huge lists">::
     ael ~skip:true (List.range 0 2_000_000)
       (fun _ -> L.append (List.range 0 1_000_000) (List.range 1_000_000 2_000_000));
   "concat of empty list of lists">::
     ael ~skip:true [] (fun _ -> L.concat []);
   "concat of normal list of lists">::
     ael ~skip:true [1;2;3;4;5;6] (fun _ -> L.concat [[1;2];[3];[];[4;5;6]]);
   "concat of huge list of small lists">::
     ael ~skip:true (List.range 0 1_000_000) 
         (fun _ -> L.concat (List.map ~f:(fun x -> [x]) (List.range 0 1_000_000)));
   "concat of small list of huge lists">::
     ael ~skip:true (List.range 0 1_000_000)
       (fun _ -> L.concat 
          (List.map ~f:(fun x -> List.range (x*100_000) ((x+1)*100_000))
             (List.range 0 10)))
  ]

let () =
  run_test_tt_main ("list-ops tests" >::: tests)
