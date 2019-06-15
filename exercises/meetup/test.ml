open OUnit2
open Meetup
open Core_kernel

type case = {
  description: string;
  year: int;
  month: int;
  week: schedule;
  dayofweek: weekday;
  dayofmonth: int
}
let sk cond =
  let skippable = try String.equal (Caml.Sys.getenv "FORCE") "false" with _ -> true in
  skip_if (skippable && cond) "Skipped"

let ae ~skip exp got _test_ctxt = 
  sk skip;
  assert_equal ~printer:(fun (y, m, d) -> sprintf "%d-%02d-%02d" y m d) exp (got ())

let make_test (c, skip) =
  c.description >::
    ae ~skip (c.year, c.month, c.dayofmonth)
       (fun _ -> meetup_day c.week c.dayofweek ~year:c.year ~month:c.month)

let tests = List.map ~f:make_test [
  ({
    description = "monteenth of May 2013";
    year = 2013;
    month = 5;
    week = Teenth;
    dayofweek = Monday;
    dayofmonth = 13
  }, true);
  ({
    description = "monteenth of August 2013";
    year = 2013;
    month = 8;
    week = Teenth;
    dayofweek = Monday;
    dayofmonth = 19
  }, true);
  ({
    description = "monteenth of September 2013";
    year = 2013;
    month = 9;
    week = Teenth;
    dayofweek = Monday;
    dayofmonth = 16
  }, true);
  ({
    description = "tuesteenth of March 2013";
    year = 2013;
    month = 3;
    week = Teenth;
    dayofweek = Tuesday;
    dayofmonth = 19
  }, true);
  ({
    description = "tuesteenth of April 2013";
    year = 2013;
    month = 4;
    week = Teenth;
    dayofweek = Tuesday;
    dayofmonth = 16
  }, true);
  ({
    description = "tuesteenth of August 2013";
    year = 2013;
    month = 8;
    week = Teenth;
    dayofweek = Tuesday;
    dayofmonth = 13
  }, true);
  ({
    description = "wednesteenth of January 2013";
    year = 2013;
    month = 1;
    week = Teenth;
    dayofweek = Wednesday;
    dayofmonth = 16
  }, true);
  ({
    description = "wednesteenth of February 2013";
    year = 2013;
    month = 2;
    week = Teenth;
    dayofweek = Wednesday;
    dayofmonth = 13
  }, true);
  ({
    description = "wednesteenth of June 2013";
    year = 2013;
    month = 6;
    week = Teenth;
    dayofweek = Wednesday;
    dayofmonth = 19
  }, true);
  ({
    description = "thursteenth of May 2013";
    year = 2013;
    month = 5;
    week = Teenth;
    dayofweek = Thursday;
    dayofmonth = 16
  }, true);
  ({
    description = "thursteenth of June 2013";
    year = 2013;
    month = 6;
    week = Teenth;
    dayofweek = Thursday;
    dayofmonth = 13
  }, true);
  ({
    description = "thursteenth of September 2013";
    year = 2013;
    month = 9;
    week = Teenth;
    dayofweek = Thursday;
    dayofmonth = 19
  }, true);
  ({
    description = "friteenth of April 2013";
    year = 2013;
    month = 4;
    week = Teenth;
    dayofweek = Friday;
    dayofmonth = 19
  }, true);
  ({
    description = "friteenth of August 2013";
    year = 2013;
    month = 8;
    week = Teenth;
    dayofweek = Friday;
    dayofmonth = 16
  }, true);
  ({
    description = "friteenth of September 2013";
    year = 2013;
    month = 9;
    week = Teenth;
    dayofweek = Friday;
    dayofmonth = 13
  }, true);
  ({
    description = "saturteenth of February 2013";
    year = 2013;
    month = 2;
    week = Teenth;
    dayofweek = Saturday;
    dayofmonth = 16
  }, true);
  ({
    description = "saturteenth of April 2013";
    year = 2013;
    month = 4;
    week = Teenth;
    dayofweek = Saturday;
    dayofmonth = 13
  }, true);
  ({
    description = "saturteenth of October 2013";
    year = 2013;
    month = 10;
    week = Teenth;
    dayofweek = Saturday;
    dayofmonth = 19
  }, true);
  ({
    description = "sunteenth of May 2013";
    year = 2013;
    month = 5;
    week = Teenth;
    dayofweek = Sunday;
    dayofmonth = 19
  }, true);
  ({
    description = "sunteenth of June 2013";
    year = 2013;
    month = 6;
    week = Teenth;
    dayofweek = Sunday;
    dayofmonth = 16
  }, true);
  ({
    description = "sunteenth of October 2013";
    year = 2013;
    month = 10;
    week = Teenth;
    dayofweek = Sunday;
    dayofmonth = 13
  }, true);
  ({
    description = "first Monday of March 2013";
    year = 2013;
    month = 3;
    week = First;
    dayofweek = Monday;
    dayofmonth = 4
  }, true);
  ({
    description = "first Monday of April 2013";
    year = 2013;
    month = 4;
    week = First;
    dayofweek = Monday;
    dayofmonth = 1
  }, true);
  ({
    description = "first Tuesday of May 2013";
    year = 2013;
    month = 5;
    week = First;
    dayofweek = Tuesday;
    dayofmonth = 7
  }, true);
  ({
    description = "first Tuesday of June 2013";
    year = 2013;
    month = 6;
    week = First;
    dayofweek = Tuesday;
    dayofmonth = 4
  }, true);
  ({
    description = "first Wednesday of July 2013";
    year = 2013;
    month = 7;
    week = First;
    dayofweek = Wednesday;
    dayofmonth = 3
  }, true);
  ({
    description = "first Wednesday of August 2013";
    year = 2013;
    month = 8;
    week = First;
    dayofweek = Wednesday;
    dayofmonth = 7
  }, true);
  ({
    description = "first Thursday of September 2013";
    year = 2013;
    month = 9;
    week = First;
    dayofweek = Thursday;
    dayofmonth = 5
  }, true);
  ({
    description = "first Thursday of October 2013";
    year = 2013;
    month = 10;
    week = First;
    dayofweek = Thursday;
    dayofmonth = 3
  }, true);
  ({
    description = "first Friday of November 2013";
    year = 2013;
    month = 11;
    week = First;
    dayofweek = Friday;
    dayofmonth = 1
  }, true);
  ({
    description = "first Friday of December 2013";
    year = 2013;
    month = 12;
    week = First;
    dayofweek = Friday;
    dayofmonth = 6
  }, true);
  ({
    description = "first Saturday of January 2013";
    year = 2013;
    month = 1;
    week = First;
    dayofweek = Saturday;
    dayofmonth = 5
  }, true);
  ({
    description = "first Saturday of February 2013";
    year = 2013;
    month = 2;
    week = First;
    dayofweek = Saturday;
    dayofmonth = 2
  }, true);
  ({
    description = "first Sunday of March 2013";
    year = 2013;
    month = 3;
    week = First;
    dayofweek = Sunday;
    dayofmonth = 3
  }, true);
  ({
    description = "first Sunday of April 2013";
    year = 2013;
    month = 4;
    week = First;
    dayofweek = Sunday;
    dayofmonth = 7
  }, true);
  ({
    description = "second Monday of March 2013";
    year = 2013;
    month = 3;
    week = Second;
    dayofweek = Monday;
    dayofmonth = 11
  }, true);
  ({
    description = "second Monday of April 2013";
    year = 2013;
    month = 4;
    week = Second;
    dayofweek = Monday;
    dayofmonth = 8
  }, true);
  ({
    description = "second Tuesday of May 2013";
    year = 2013;
    month = 5;
    week = Second;
    dayofweek = Tuesday;
    dayofmonth = 14
  }, true);
  ({
    description = "second Tuesday of June 2013";
    year = 2013;
    month = 6;
    week = Second;
    dayofweek = Tuesday;
    dayofmonth = 11
  }, true);
  ({
    description = "second Wednesday of July 2013";
    year = 2013;
    month = 7;
    week = Second;
    dayofweek = Wednesday;
    dayofmonth = 10
  }, true);
  ({
    description = "second Wednesday of August 2013";
    year = 2013;
    month = 8;
    week = Second;
    dayofweek = Wednesday;
    dayofmonth = 14
  }, true);
  ({
    description = "second Thursday of September 2013";
    year = 2013;
    month = 9;
    week = Second;
    dayofweek = Thursday;
    dayofmonth = 12
  }, true);
  ({
    description = "second Thursday of October 2013";
    year = 2013;
    month = 10;
    week = Second;
    dayofweek = Thursday;
    dayofmonth = 10
  }, true);
  ({
    description = "second Friday of November 2013";
    year = 2013;
    month = 11;
    week = Second;
    dayofweek = Friday;
    dayofmonth = 8
  }, true);
  ({
    description = "second Friday of December 2013";
    year = 2013;
    month = 12;
    week = Second;
    dayofweek = Friday;
    dayofmonth = 13
  }, true);
  ({
    description = "second Saturday of January 2013";
    year = 2013;
    month = 1;
    week = Second;
    dayofweek = Saturday;
    dayofmonth = 12
  }, true);
  ({
    description = "second Saturday of February 2013";
    year = 2013;
    month = 2;
    week = Second;
    dayofweek = Saturday;
    dayofmonth = 9
  }, true);
  ({
    description = "second Sunday of March 2013";
    year = 2013;
    month = 3;
    week = Second;
    dayofweek = Sunday;
    dayofmonth = 10
  }, true);
  ({
    description = "second Sunday of April 2013";
    year = 2013;
    month = 4;
    week = Second;
    dayofweek = Sunday;
    dayofmonth = 14
  }, true);
  ({
    description = "third Monday of March 2013";
    year = 2013;
    month = 3;
    week = Third;
    dayofweek = Monday;
    dayofmonth = 18
  }, true);
  ({
    description = "third Monday of April 2013";
    year = 2013;
    month = 4;
    week = Third;
    dayofweek = Monday;
    dayofmonth = 15
  }, true);
  ({
    description = "third Tuesday of May 2013";
    year = 2013;
    month = 5;
    week = Third;
    dayofweek = Tuesday;
    dayofmonth = 21
  }, true);
  ({
    description = "third Tuesday of June 2013";
    year = 2013;
    month = 6;
    week = Third;
    dayofweek = Tuesday;
    dayofmonth = 18
  }, true);
  ({
    description = "third Wednesday of July 2013";
    year = 2013;
    month = 7;
    week = Third;
    dayofweek = Wednesday;
    dayofmonth = 17
  }, true);
  ({
    description = "third Wednesday of August 2013";
    year = 2013;
    month = 8;
    week = Third;
    dayofweek = Wednesday;
    dayofmonth = 21
  }, true);
  ({
    description = "third Thursday of September 2013";
    year = 2013;
    month = 9;
    week = Third;
    dayofweek = Thursday;
    dayofmonth = 19
  }, true);
  ({
    description = "third Thursday of October 2013";
    year = 2013;
    month = 10;
    week = Third;
    dayofweek = Thursday;
    dayofmonth = 17
  }, true);
  ({
    description = "third Friday of November 2013";
    year = 2013;
    month = 11;
    week = Third;
    dayofweek = Friday;
    dayofmonth = 15
  }, true);
  ({
    description = "third Friday of December 2013";
    year = 2013;
    month = 12;
    week = Third;
    dayofweek = Friday;
    dayofmonth = 20
  }, true);
  ({
    description = "third Saturday of January 2013";
    year = 2013;
    month = 1;
    week = Third;
    dayofweek = Saturday;
    dayofmonth = 19
  }, true);
  ({
    description = "third Saturday of February 2013";
    year = 2013;
    month = 2;
    week = Third;
    dayofweek = Saturday;
    dayofmonth = 16
  }, true);
  ({
    description = "third Sunday of March 2013";
    year = 2013;
    month = 3;
    week = Third;
    dayofweek = Sunday;
    dayofmonth = 17
  }, true);
  ({
    description = "third Sunday of April 2013";
    year = 2013;
    month = 4;
    week = Third;
    dayofweek = Sunday;
    dayofmonth = 21
  }, true);
  ({
    description = "fourth Monday of March 2013";
    year = 2013;
    month = 3;
    week = Fourth;
    dayofweek = Monday;
    dayofmonth = 25
  }, true);
  ({
    description = "fourth Monday of April 2013";
    year = 2013;
    month = 4;
    week = Fourth;
    dayofweek = Monday;
    dayofmonth = 22
  }, true);
  ({
    description = "fourth Tuesday of May 2013";
    year = 2013;
    month = 5;
    week = Fourth;
    dayofweek = Tuesday;
    dayofmonth = 28
  }, true);
  ({
    description = "fourth Tuesday of June 2013";
    year = 2013;
    month = 6;
    week = Fourth;
    dayofweek = Tuesday;
    dayofmonth = 25
  }, true);
  ({
    description = "fourth Wednesday of July 2013";
    year = 2013;
    month = 7;
    week = Fourth;
    dayofweek = Wednesday;
    dayofmonth = 24
  }, true);
  ({
    description = "fourth Wednesday of August 2013";
    year = 2013;
    month = 8;
    week = Fourth;
    dayofweek = Wednesday;
    dayofmonth = 28
  }, true);
  ({
    description = "fourth Thursday of September 2013";
    year = 2013;
    month = 9;
    week = Fourth;
    dayofweek = Thursday;
    dayofmonth = 26
  }, true);
  ({
    description = "fourth Thursday of October 2013";
    year = 2013;
    month = 10;
    week = Fourth;
    dayofweek = Thursday;
    dayofmonth = 24
  }, true);
  ({
    description = "fourth Friday of November 2013";
    year = 2013;
    month = 11;
    week = Fourth;
    dayofweek = Friday;
    dayofmonth = 22
  }, true);
  ({
    description = "fourth Friday of December 2013";
    year = 2013;
    month = 12;
    week = Fourth;
    dayofweek = Friday;
    dayofmonth = 27
  }, true);
  ({
    description = "fourth Saturday of January 2013";
    year = 2013;
    month = 1;
    week = Fourth;
    dayofweek = Saturday;
    dayofmonth = 26
  }, true);
  ({
    description = "fourth Saturday of February 2013";
    year = 2013;
    month = 2;
    week = Fourth;
    dayofweek = Saturday;
    dayofmonth = 23
  }, true);
  ({
    description = "fourth Sunday of March 2013";
    year = 2013;
    month = 3;
    week = Fourth;
    dayofweek = Sunday;
    dayofmonth = 24
  }, true);
  ({
    description = "fourth Sunday of April 2013";
    year = 2013;
    month = 4;
    week = Fourth;
    dayofweek = Sunday;
    dayofmonth = 28
  }, true);
  ({
    description = "last Monday of March 2013";
    year = 2013;
    month = 3;
    week = Last;
    dayofweek = Monday;
    dayofmonth = 25
  }, true);
  ({
    description = "last Monday of April 2013";
    year = 2013;
    month = 4;
    week = Last;
    dayofweek = Monday;
    dayofmonth = 29
  }, true);
  ({
    description = "last Tuesday of May 2013";
    year = 2013;
    month = 5;
    week = Last;
    dayofweek = Tuesday;
    dayofmonth = 28
  }, true);
  ({
    description = "last Tuesday of June 2013";
    year = 2013;
    month = 6;
    week = Last;
    dayofweek = Tuesday;
    dayofmonth = 25
  }, true);
  ({
    description = "last Wednesday of July 2013";
    year = 2013;
    month = 7;
    week = Last;
    dayofweek = Wednesday;
    dayofmonth = 31
  }, true);
  ({
    description = "last Wednesday of August 2013";
    year = 2013;
    month = 8;
    week = Last;
    dayofweek = Wednesday;
    dayofmonth = 28
  }, true);
  ({
    description = "last Thursday of September 2013";
    year = 2013;
    month = 9;
    week = Last;
    dayofweek = Thursday;
    dayofmonth = 26
  }, true);
  ({
    description = "last Thursday of October 2013";
    year = 2013;
    month = 10;
    week = Last;
    dayofweek = Thursday;
    dayofmonth = 31
  }, true);
  ({
    description = "last Friday of November 2013";
    year = 2013;
    month = 11;
    week = Last;
    dayofweek = Friday;
    dayofmonth = 29
  }, true);
  ({
    description = "last Friday of December 2013";
    year = 2013;
    month = 12;
    week = Last;
    dayofweek = Friday;
    dayofmonth = 27
  }, true);
  ({
    description = "last Saturday of January 2013";
    year = 2013;
    month = 1;
    week = Last;
    dayofweek = Saturday;
    dayofmonth = 26
  }, true);
  ({
    description = "last Saturday of February 2013";
    year = 2013;
    month = 2;
    week = Last;
    dayofweek = Saturday;
    dayofmonth = 23
  }, true);
  ({
    description = "last Sunday of March 2013";
    year = 2013;
    month = 3;
    week = Last;
    dayofweek = Sunday;
    dayofmonth = 31
  }, true);
  ({
    description = "last Sunday of April 2013";
    year = 2013;
    month = 4;
    week = Last;
    dayofweek = Sunday;
    dayofmonth = 28
  }, true);
  ({
    description = "last Wednesday of February 2012";
    year = 2012;
    month = 2;
    week = Last;
    dayofweek = Wednesday;
    dayofmonth = 29
  }, true);
  ({
    description = "last Wednesday of December 2014";
    year = 2014;
    month = 12;
    week = Last;
    dayofweek = Wednesday;
    dayofmonth = 31
  }, true);
  ({
    description = "last Sunday of February 2015";
    year = 2015;
    month = 2;
    week = Last;
    dayofweek = Sunday;
    dayofmonth = 22
  }, true);
  ({
    description = "first Friday of December 2012";
    year = 2012;
    month = 12;
    week = First;
    dayofweek = Friday;
    dayofmonth = 7
  }, true)
]

let () =
  run_test_tt_main ("meetup tests" >::: tests)
