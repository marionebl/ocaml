open Base
open OUnit2
open Word_count
let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt =
  sk skip;
  let cmp = Map.equal (=) in
  let sexp_of_map = Map.sexp_of_m__t (module String) in
  let printer m = sexp_of_map Int.sexp_of_t m |> Sexp.to_string_hum ~indent:1 in
  assert_equal ((Map.of_alist_exn (module String)) exp) (got ()) ~cmp ~printer

let tests = [
   "count one word" >::
      ae ~skip:true [("word", 1)]
         (fun _ -> word_count "word");
   "count one of each word" >::
      ae ~skip:true [("one", 1); ("of", 1); ("each", 1)]
         (fun _ -> word_count "one of each");
   "multiple occurrences of a word" >::
      ae ~skip:true [("one", 1); ("fish", 4); ("two", 1); ("red", 1); ("blue", 1)]
         (fun _ -> word_count "one fish two fish red fish blue fish");
   "handles cramped lists" >::
      ae ~skip:true [("one", 1); ("two", 1); ("three", 1)]
         (fun _ -> word_count "one,two,three");
   "handles expanded lists" >::
      ae ~skip:true [("one", 1); ("two", 1); ("three", 1)]
         (fun _ -> word_count "one,\ntwo,\nthree");
   "ignore punctuation" >::
      ae ~skip:true [("car", 1); ("carpet", 1); ("as", 1); ("java", 1); ("javascript", 1)]
         (fun _ -> word_count "car: carpet as java: javascript!!&@$%^&");
   "include numbers" >::
      ae ~skip:true [("testing", 2); ("1", 1); ("2", 1)]
         (fun _ -> word_count "testing, 1, 2 testing");
   "normalize case" >::
      ae ~skip:true [("go", 3); ("stop", 2)]
         (fun _ -> word_count "go Go GO Stop stop");
   "with apostrophes" >::
      ae ~skip:true [("first", 1); ("don't", 2); ("laugh", 1); ("then", 1); ("cry", 1)]
         (fun _ -> word_count "First: don't laugh. Then: don't cry.");
   "with quotations" >::
      ae ~skip:true [("joe", 1); ("can't", 1); ("tell", 1); ("between", 1); ("large", 2); ("and", 1)]
         (fun _ -> word_count "Joe can't tell between 'large' and large.");
   "multiple spaces not detected as a word" >::
      ae ~skip:true [("multiple", 1); ("whitespaces", 1)]
         (fun _ -> word_count " multiple   whitespaces");
]

let () =
  run_test_tt_main ("word_count tests" >::: tests)
