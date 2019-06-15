let leap_year = function
    | y when y mod 400 = 0 -> true
    | y when y mod 100 = 0 -> false
    | y -> y mod 4 = 0