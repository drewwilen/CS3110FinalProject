type key
(** key is used to identify stocks (typically string for a ticker) *)

type stock
(** stock is the type for stocks - has a ticker and a value ref *)

type value
(** value is what is contained in that portfolio for a certain stock, including
    shares owned, gain, etc.*)

type t
(** t is the type for stock portfolios. *)

type user

(** A user is a type that has a username, password, and portfolio associated
    with it *)
type users
(** [users] keeps track of multiple users, including passwords, usernames, and
    portfolio *)

val make_user : string -> string -> user
(** [make_user username password] Creates a user with username [username],
    password [password], the empty portfolio *)

val empty_users : users
(* [empty_users] is an empty set of users that can then be added to *)

val add_user : user -> users -> users
(* [add_users u users] adds u to the group of users that have portfolios*)

val login_attempt : string -> string -> users -> t option
(* [login_attempt u p user] returns None if the password and username are not
   found in the users, and returns some portfolio if it is found in the users *)

val make_user_from_portfolio : string -> string -> t -> user
(* [make_user username password portfolio] Creates a user with username
   [username], password [password], and the portfolio [portfolio] *)

val get_portfolio : user -> t
(* [get_portfolio a] returns the portfolio of a given user *)

val empty : t
(** [empty] Initializes an empty portfolio*)

val is_empty : t -> bool
(** [is_empty portfolio] Returns whether or not the portfolio is empty*)

val num_stocks : t -> int
(** [num_stocks portfolio] returns the number of unique stocks in the portfolio*)

val num_shares : t -> int
(** [num_shares portfolio] returns the number of shares for every stock in the
    portfolio*)

val stocks : t -> string
(** The equivalent of a to_string function. Takes in a portfolio and returns a
    string of the stocks in that portfolio*)

val print_portfolio : t -> unit
(* prints out the portfolio, including all the information of stocks and prices.
   Tested the main program *)

val lookup_gain : string -> t -> float option
(** [lookup_gain ticker portfolio] Takes in a ticker and a portfolio. Outputs
    the raw gain value if the stock is in the portfolio, None if nothing *)

val value : t -> float
(** [value portfolio] Takes in a portfolio and outputs the total value of stocks
    contained*)

val make_stock : string -> float -> stock
(* Initialize a stock into acceptable format for buying and selling *)

val buy : stock -> int -> t -> t
(** Purchase a amount of stocks and add that to an portfolio. The equivalent of
    add *)

val sell : string -> int -> t -> t
(** Sell an amount of stocks in the existing portfolio. If the stock is not in
    the portfolio, returns unchanged portfolio*)

val update_stock : stock -> float -> float
(** Updates the price of a stock. Portfolios that contain it will be updated
    too. Returns the updated price *)

val update : t -> t
(** Updates portfolio statistics to match potentially updated stocks*)

val to_backtest : t -> (string * int) list
(** From a portfolio, returns an association list of string * int of the tickers
    you have in the portfolio, and how many shares you have invested in it *)
