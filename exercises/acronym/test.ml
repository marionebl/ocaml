open OUnit2
open Acronym

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ?(skip=false) exp got _test_ctxt  =
  sk skip;
  assert_equal exp (got ()) ~printer:(fun x -> x)

let tests = [
  "basic" >::
    ae ~skip:true "PNG" (fun _ -> acronym "Portable Network Graphics");
  "lowercase words" >::
    ae ~skip:true "ROR" (fun _ -> acronym "Ruby on Rails");
  "punctuation" >::
    ae ~skip:true "FIFO" (fun _ -> acronym "First In, First Out");
  "all caps word" >::
    ae ~skip:true "GIMP" (fun _ -> acronym "GNU Image Manipulation Program");
  "punctuation without whitespace" >::
    ae ~skip:true "CMOS" (fun _ -> acronym "Complementary metal-oxide semiconductor");
  "very long abbreviation" >::
    ae ~skip:true "ROTFLSHTMDCOALM" (fun _ -> acronym "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me");
  "consecutive delimiters" >::
    ae ~skip:true "SIMUFTA" (fun _ -> acronym "Something - I made up from thin air");
]

let () =
  run_test_tt_main ("acronym tests" >::: tests)
