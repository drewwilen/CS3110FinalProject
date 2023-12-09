(* type input

   (* A list containing all valid stocks that can be backtested from 2013 to
   2018 *) val s_and_p : string list

   (* A function that takes in a stock in the form of a ticker/string, an amount
   of time, an interval of time and outputs the gain/loss of that stock over
   each interval *) val stock_performance : string -> int -> int -> float

   (* A function that takes in an input an amount of time, an interval of time
   and outputs the gain/loss of that stock over each interval *) val
   test_portfolio : input -> int -> float *)
