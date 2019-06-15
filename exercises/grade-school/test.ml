
open OUnit2
open Grade_school

let sk cond =
  let skippable = try String.equal (Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let assert_lists_equal ~skip exp got _ctx =
  let printer = String.concat ";" in
  sk skip;
  assert_equal exp (got ()) ~printer

let tests = [
  "an empty school has no children in grade 1" >::
    assert_lists_equal ~skip:true [] (fun _ -> grade 1 empty_school);
  "an empty school has Emma in grade 1 after she's been added to grade 1" >::(
    assert_lists_equal ~skip:true ["Emma"] (fun _ -> 
      let s = add "Emma" 1 empty_school in
      grade 1 s)
  );
  "an empty school does not have Emma in grade 1 after she's been added to grade 2" >::(
    assert_lists_equal ~skip:true [] (fun _ -> 
      let s = add "Emma" 2 empty_school in
      grade 1 s)
  );
  "an empty school has Emma, Timmy, Bob and Becky in grade 1 after they've been added to grade 1" >::(
    assert_lists_equal ~skip:true ["Becky"; "Bob"; "Emma"; "Timmy"] (fun _ -> 
      let s = empty_school |> add "Emma" 1 |> add "Timmy" 1 |> add "Bob" 1 |> add "Becky" 1 in
      grade 1 s |> List.sort compare)
  );
  "a sorted school has Emma, Timmy, Bob and Becky in grade 1 in sorted order" >::(
    assert_lists_equal ~skip:true ["Becky"; "Bob"; "Emma"; "Timmy"] (fun _ -> 
      let s = empty_school |> add "Emma" 1 |> add "Timmy" 1 |> add "Bob" 1 |> add "Becky" 1 |> sorted in
      grade 1 s)
  );
]

let () =
  run_test_tt_main ("grade-school tests" >::: tests)
