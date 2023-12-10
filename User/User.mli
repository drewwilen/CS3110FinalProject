type key
type stock
type value
type t
type user
type users

(* val portfolio : t (** A hash table where the Key represents a stock and the
   value is a record with the relevant data*) *)

val make_user : string -> string -> user
(* [make_user username password] Creates a user with username [username],
   password [password], the empty portfolio *)

val get_portfolio : user -> t
(* [get_portfolio a] returns the portfolio of a given user *)

val empty : t
(** Initializes an empty portfolio*)

val is_empty : t -> bool
(** Returns whether or not the portfolio is empty*)

val stocks : t -> string
(** The equivalent of a to_string function. Takes in a portfolio and returns a
    string of the stocks in that portfolio*)

val print_portfolio : t -> unit
(* prints out the portfolio *)

val lookup_gain : string -> t -> float option
(** Takes in a key and a portfolio. Outputs the raw gain value if the stock is
    in the portfolio, None if nothing *)

val value : t -> float
(** Takes in a portfolio and outputs the total value of stocks contained*)

val make_stock : string -> float -> stock
(* Initialize a stock into acceptable format for buying and selling *)

val buy : stock -> int -> t -> t
(** Purchase a amount of stocks and add that to an portfolio. The equivalent of
    add *)

val sell : stock -> int -> t -> t
(** Sell an amount of stocks in the existing portfolio. The equivalent of remove*)

val update : stock -> t -> t
(** Updates the information about a stock to update a certain portfolio*)

val to_backtest : t -> (string * int) list
(** From a portfolio, returns an association list of string * int of the tickers
    you have in the portfolio, and how many shares you have invested in it *)
