open Base
open OUnit2
open Hamming

let printer = function
| None -> "None"
| Some x -> Int.to_string x

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
   sk skip;
   assert_equal ~printer exp (got ())

let dna_of_string s =
  let f = function
    | 'A' -> A
    | 'C' -> C
    | 'G' -> G
    | 'T' -> T
    | _   -> failwith "Big news! New nucleotide discovered" in
  String.to_list s |> List.map ~f

let hamdist a b = hamming_distance (dna_of_string a) (dna_of_string b)

let tests = [
   "empty strands" >::
      ae ~skip:false (Some 0) (fun _ -> hamdist "" "");
   "identical strands" >::
      ae ~skip:false (Some 0) (fun _ -> hamdist "A" "A");
   "long identical strands" >::
      ae ~skip:false (Some 0) (fun _ -> hamdist "GGACTGA" "GGACTGA");
   "complete distance in single nucleotide strands" >::
      ae ~skip:false (Some 1) (fun _ -> hamdist "A" "G");
   "complete distance in small strands" >::
      ae ~skip:false (Some 2) (fun _ -> hamdist "AG" "CT");
   "small distance in small strands" >::
      ae ~skip:false (Some 1) (fun _ -> hamdist "AT" "CT");
   "small distance" >::
      ae ~skip:false (Some 1) (fun _ -> hamdist "GGACG" "GGTCG");
   "small distance in long strands" >::
      ae ~skip:false (Some 2) (fun _ -> hamdist "ACCAGGG" "ACTATGG");
   "non-unique character in first strand" >::
      ae ~skip:false (Some 1) (fun _ -> hamdist "AAG" "AAA");
   "non-unique character in second strand" >::
      ae ~skip:false (Some 1) (fun _ -> hamdist "AAA" "AAG");
   "same nucleotides in different positions" >::
      ae ~skip:false (Some 2) (fun _ -> hamdist "TAG" "GAT");
   "large distance" >::
      ae ~skip:false (Some 4) (fun _ -> hamdist "GATACA" "GCATAA");
   "large distance in off-by-one strand" >::
      ae ~skip:false (Some 9) (fun _ -> hamdist "GGACGGATTCTG" "AGGACGGATTCT");
   "disallow first strand longer" >::
      ae ~skip:false None (fun _ -> hamdist "AATG" "AAA");
   "disallow second strand longer" >::
      ae ~skip:false None (fun _ -> hamdist "ATA" "AGTG");
]

let () =
  run_test_tt_main ("hamming tests" >::: tests)
