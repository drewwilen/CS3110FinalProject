val read_csv_file : string -> string list list
(** Function to read a CSV file. Takes in a file path and outputs the data
    contained in the csv file *)

val s_and_p : string list
(** A list that contains all tickers of stocks in the S&P 500 *)

val create_file_path_individual : string -> string
(** Function that takes a string (a ticker) and creates the file path that
    directs you to that csv file*)

val create_list : string -> string list list
(** Given a ticker, create_list will read the csv and convert the data into a
    string list list. Each list contained in the list contains stock data for
    that particular day*)

val print_data : string list list -> unit
(** Given a list of stock data, print_data will output all relevant data. This
    includes the following: date, open, high, low, close, volume, name*)

val valid_stocks : string list -> string list
(** Given a list of stocks, valid_stocks will make sure these stocks are
    contained in the S&P 500*)

val first_n_elements : 'a list -> int -> 'a list
(** Given a list and an integer n, first_n_elements will output that list with
    the first n elements*)

val historical_stock_data : string -> int -> int -> string list list
(** Function that takes in a ticker, an amount of days, and an interval. This
    function will create a list of the closing prices of the number of days
    after 2013-02-08. Note that the number of days and intervals cannot exceed
    1259. The interval will divide up this number of days and will also be the
    number of elements in the output list.

    Example: backtest_stock AAPL 100 10 will look at 100 days of apple and
    record the closing price on every 10th day. Thus the output will have 10
    elements*)

val historial_n_stocks_data : string list -> int -> int -> string list list list
(** Function that takes in a list of tickers, an amount of days, and an
    interval. This function will ouput a list of what historical_stock_data
    ouputs. See that specification as the same rules apply*)

val change_of_stock : string -> int -> float
(** Function that calculates the change in close price between any interval of
    days. Days must be greater than 1 *)

val change_of_portfolio : string list -> int -> float
(** Function that calculates the change in close price between any interval of
    days for a portfolio of stocks *)

val change_of_portfolio_adjust : (string * int) list -> int -> float
(** Function that calculates the change in close price between any interval of
    days for a portfolio of stocks, and adjusts for the number of shares owned*)
