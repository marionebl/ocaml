open Base
open OUnit2

module NC = Nucleotide_count

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

(* Assert that two 'int option' values are equivalent. *)
let aire ~skip exp got _ctxt =
  sk skip;
  let printer m =
    Result.sexp_of_t
      Int.sexp_of_t
      Char.sexp_of_t
      m
    |> Sexp.to_string_hum ~indent:1
  in assert_equal exp (got ()) ~printer

(* Assert that two '(int Char.Map.t, char) Result.t' values are equivalent. *)
let amre ~skip exp got _ctxt =
  sk skip;
  let sexp_of_map = Map.sexp_of_m__t (module Char) in
  let printer m =
    Result.sexp_of_t (sexp_of_map Int.sexp_of_t) Char.sexp_of_t m
    |> Sexp.to_string_hum ~indent:1
  in
  let cmp exp got = match exp, got with
    | Ok exp_map, Ok got_map -> Map.equal Int.equal exp_map got_map
    | Error c1, Error c2     -> Char.equal c1 c2
    | _ -> false
  in assert_equal exp (got ()) ~cmp ~printer

let tests =
  [ "Empty DNA string has no invalid nucleotides" >:: aire ~skip:true (Error 'X') (fun _ -> NC.count_nucleotide "" 'X');
    "Non-empty DNA string has no invalid nucleotides" >:: aire ~skip:true (Error 'X') (fun _ -> NC.count_nucleotide "ACGT" 'X');
    "Invalid DNA string has no invalid nucleotides" >:: aire ~skip:true (Error 'X') (fun _ -> NC.count_nucleotide "ACGXT" 'A');

    "Empty DNA string has zero Adenine nucleotides" >:: aire ~skip:true (Ok 0) (fun _ -> NC.count_nucleotide "" 'A');
    "DNA string with one Adenine nucleotide" >:: aire ~skip:true (Ok 1) (fun _ -> NC.count_nucleotide "A" 'A');
    "DNA string with five Cytosine nucleotides" >:: aire ~skip:true (Ok 5) (fun _ -> NC.count_nucleotide "CCCCC" 'C');
    "DNA string with two Guanine nucleotides" >:: aire ~skip:true (Ok 2) (fun _ -> NC.count_nucleotide "ACGGT" 'G');
    "DNA string with three Thymine nucleotides" >:: aire ~skip:true (Ok 3) (fun _ -> NC.count_nucleotide "CACTAGCTGCT" 'T');

    "Invalid DNA string has no nucleotides" >::
      amre ~skip:true (Error 'X') (fun _ -> NC.count_nucleotides "ACGXT");

    "Empty DNA string has zero nucleotides" >::
      amre ~skip:true (Ok (Map.empty (module Char))) (fun _ -> NC.count_nucleotides "");

    "DNA string with two Adenine nucleotides" >::
      amre ~skip:true (Ok (Map.singleton (module Char) 'A' 2)) (fun _ -> NC.count_nucleotides "AA");

    "DNA string with one Adenine, two Cytosine nucleotides" >::
      begin
        let exp = Ok ((Map.of_alist_exn (module Char)) [('A', 1); ('C', 2)])
        in amre ~skip:true exp (fun _ -> NC.count_nucleotides "ACC")
      end;

    "DNA string with one Adenine, two Cytosine, three Guanine, four Thymine nucleotides" >::
      begin
        let exp = Ok ((Map.of_alist_exn (module Char)) [('A', 1); ('C', 2); ('G', 3); ('T', 4)])
        in amre ~skip:true exp (fun _ -> NC.count_nucleotides "CGTATGTCTG")
      end;
  ]

let () =
  run_test_tt_main ("nucleotide-counts tests" >::: tests)
