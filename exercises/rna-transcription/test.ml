open Base
open OUnit2

let char_of_variant = function
  | `A -> 'A' | `C -> 'C' | `G -> 'G' | `T -> 'T' | `U -> 'U'

let printer l = List.map ~f:char_of_variant l |> String.of_char_list

let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
  sk skip;
  assert_equal ~printer exp (got ())

let tests =
  ["transcribes empty list">::
    ae ~skip:true [] (fun _ -> Rna_transcription .to_rna []);
   "transcribes cytidine">::
    ae ~skip:true [`G] (fun _ -> Rna_transcription .to_rna [`C]);
   "transcribes guanosine">::
    ae ~skip:true [`C] (fun _ -> Rna_transcription .to_rna [`G]);
   "transcribes adenosie">::
    ae ~skip:true [`U] (fun _ -> Rna_transcription .to_rna [`A]);
   "transcribes thymidine">::
    ae ~skip:true [`A] (fun _ -> Rna_transcription .to_rna [`T]);
   "transcribes multiple">::
    ae ~skip:true [`U; `G; `C; `A; `C; `C; `A; `G; `A; `A; `U; `U]
       (fun _ -> Rna_transcription .to_rna [`A; `C; `G; `T; `G; `G; `T; `C; `T; `T; `A; `A])
  ]

let () =
  run_test_tt_main ("rna-transcription tests" >::: tests)
