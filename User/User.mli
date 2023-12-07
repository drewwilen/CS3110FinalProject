type key
type stock
type value
type t

(* val portfolio : t (** A hash table where the Key represents a stock and the
   value is a record with the relevant data*) *)

val empty : t
(** Initializes an empty portfolio*)

val is_empty : t -> bool
(** Returns whether or not the portfolio is empty*)

val stocks : t -> string list
(** The equivalent of a to_string function. Takes in a portfolio and returns a
    string of the stocks in that portfolio*)

val print_portfolio : t -> unit
(* prints out the portfolio *)

val lookup : key -> t -> value option
(** Takes in a key and a portfolio. Outputs some value if the stock is in the
    portfolio, None if nothing *)

val value : t -> int
(** Takes in a portfolio and outputs the total value of stocks contained*)

val make_stock : string -> float -> stock
(* Initialize a stock into acceptable format for buying and selling *)

val buy : stock -> int -> t -> t
(** Purchase a amount of stocks and add that to an portfolio. The equivalent of
    add *)

val sell : key -> int -> t -> t
(** Sell an amount of stocks in the existing portfolio. The equivalent of remove*)

val update : t -> t
(** Updates the information contained in a value *)
