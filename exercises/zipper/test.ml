open Base
open OUnit2

module T = Tree
module Z = Zipper

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

(* Assert Equals Int *)
let aei ~skip exp got _test_ctxt =
  sk skip;
  assert_equal exp (got ()) ~cmp:Int.equal ~printer:Int.to_string

(* Assert Equals Tree *)
let aet ~skip exp got _test_ctxt =
  sk skip;
  let printer t = Tree.sexp_of_t Int.sexp_of_t t |> Sexp.to_string_hum in
  assert_equal (exp ()) (got ()) ~cmp:(Tree.equal Int.equal) ~printer

(* Assert Equals Tree Option *)
let aeto ~skip exp got _test_ctxt =
  sk skip;
  let printer ot = Option.sexp_of_t (Tree.sexp_of_t Int.sexp_of_t) ot
                   |> Sexp.to_string_hum in
  assert_equal exp (got ()) ~cmp:(Option.equal (Tree.equal Int.equal)) ~printer

(* Assert Equals Zipper Option *)
let aezo ~skip exp got _test_ctxt =
  sk skip;
  let printer ot = Option.sexp_of_t (Zipper.sexp_of_t Int.sexp_of_t) ot
                   |> Sexp.to_string_hum in
  assert_equal (exp ()) (got ()) ~cmp:(Option.equal (Zipper.equal Int.equal)) ~printer

let tree value left right = { T.value; left; right }
let node value left right = Some { T.value; left; right }
let leaf value            = Some { T.value; left = None; right = None }

let t1 = tree 1 (node 2 None     (leaf 3)) (leaf 4)
let t2 = tree 1 (node 5 None     (leaf 3)) (leaf 4)
let t3 = tree 1 (node 2 (leaf 5) (leaf 3)) (leaf 4)
let t4 = tree 1 (leaf 2)                   (leaf 4)

(* Unwrap Option *)
let uo o = Option.value_exn o

let tests =
  ["data is retained">::
     aet ~skip:true (fun _ -> Z.of_tree t1 |> Z.to_tree) (fun _ -> t1);
   "left, right and value">::
     aei ~skip:true 3 (fun _ ->  Z.of_tree t1 |> Z.left |> uo |> Z.right |> uo |> Z.value);
   "dead end">::
     aezo ~skip:true (fun _ -> None) (fun _ ->  Z.of_tree t1 |> Z.left |> uo |> Z.left);
   "tree from deep focus">::
     aet ~skip:true (fun _ -> t1) (fun _ -> Z.of_tree t1 |> Z.left |> uo |> Z.right |> uo |> Z.to_tree);
   "set_value">::
     aet ~skip:true (fun _ -> t2) (fun _ -> Z.of_tree t1 |> Z.left |> uo |> Z.set_value 5 |> Z.to_tree);
   "set_left with Some">::
     aet ~skip:true (fun _ -> t3) (fun _ -> Z.of_tree t1 |> Z.left |> uo |> Z.set_left (leaf 5) |> Z.to_tree);
   "set_right with None">::
     aet ~skip:true (fun _ -> t4) (fun _ -> Z.of_tree t1 |> Z.left |> uo |> Z.set_right None |> Z.to_tree);
   "different paths to same zipper">::
     aezo ~skip:true (fun _ -> Z.of_tree t1 |> Z.right)
       (fun _ -> Z.of_tree t1 |> Z.left |> uo |> Z.up |> uo |> Z.right);
  ]

let () =
  run_test_tt_main ("zipper tests" >::: tests)
