open OUnit2
open Bob

let sk cond =
  let skippable = try String.equal (Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ?(skip=false) exp got _test_ctxt = 
   sk skip;
   assert_equal ~printer:(fun x -> x) exp (got ())

let tests = [
   "stating something" >::
      ae ~skip:true "Whatever." (fun _ -> response_for "Tom-ay-to, tom-aaaah-to.");
   "shouting" >::
      ae ~skip:true "Whoa, chill out!" (fun _ -> response_for "WATCH OUT!");
   "shouting gibberish" >::
      ae ~skip:true "Whoa, chill out!" (fun _ -> response_for "FCECDFCAAB");
   "asking a question" >::
      ae ~skip:true "Sure." (fun _ -> response_for "Does this cryogenic chamber make me look fat?");
   "asking a numeric question" >::
      ae ~skip:true "Sure." (fun _ -> response_for "You are, what, like 15?");
   "asking gibberish" >::
      ae ~skip:true "Sure." (fun _ -> response_for "fffbbcbeab?");
   "talking forcefully" >::
      ae ~skip:true "Whatever." (fun _ -> response_for "Let's go make out behind the gym!");
   "using acronyms in regular speech" >::
      ae ~skip:true "Whatever." (fun _ -> response_for "It's OK if you don't want to go to the DMV.");
   "forceful question" >::
      ae ~skip:true "Calm down, I know what I'm doing!" (fun _ -> response_for "WHAT THE HELL WERE YOU THINKING?");
   "shouting numbers" >::
      ae ~skip:true "Whoa, chill out!" (fun _ -> response_for "1, 2, 3 GO!");
   "only numbers" >::
      ae ~skip:true "Whatever." (fun _ -> response_for "1, 2, 3");
   "question with only numbers" >::
      ae ~skip:true "Sure." (fun _ -> response_for "4?");
   "shouting with special characters" >::
      ae ~skip:true "Whoa, chill out!" (fun _ -> response_for "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!");
   "shouting with no exclamation mark" >::
      ae ~skip:true "Whoa, chill out!" (fun _ -> response_for "I HATE YOU");
   "statement containing question mark" >::
      ae ~skip:true "Whatever." (fun _ -> response_for "Ending with ? means a question.");
   "non-letters with question" >::
      ae ~skip:true "Sure." (fun _ -> response_for ":) ?");
   "prattling on" >::
      ae ~skip:true "Sure." (fun _ -> response_for "Wait! Hang on. Are you going to be OK?");
   "silence" >::
      ae ~skip:true "Fine. Be that way!" (fun _ -> response_for "");
   "prolonged silence" >::
      ae ~skip:true "Fine. Be that way!" (fun _ -> response_for "          ");
   "alternate silence" >::
      ae ~skip:true "Fine. Be that way!" (fun _ -> response_for "\t\t\t\t\t\t\t\t\t\t");
   "multiple line question" >::
      ae ~skip:true "Whatever." (fun _ -> response_for "\nDoes this cryogenic chamber make me look fat?\nno");
   "starting with whitespace" >::
      ae ~skip:true "Whatever." (fun _ -> response_for "         hmmmmmmm...");
   "ending with whitespace" >::
      ae ~skip:true "Sure." (fun _ -> response_for "Okay if like my  spacebar  quite a bit?   ");
   "other whitespace" >::
      ae ~skip:true "Fine. Be that way!" (fun _ -> response_for "\n\r \t");
   "non-question ending with whitespace" >::
      ae ~skip:true "Whatever." (fun _ -> response_for "This is a statement ending with whitespace      ");
]

let () =
  run_test_tt_main ("bob tests" >::: tests)
