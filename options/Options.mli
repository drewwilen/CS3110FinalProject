(* options_trading.ml *)

type option_type =
  | Call
  | Put

type option_contract = {
  underlying_price : float;
  strike_price : float;
  time_to_expiry : float;
  volatility : float;
  interest_rate : float;
  option_type : option_type;
  steps : int;
}
(** Define a record type for representing an options contract *)

val print_underlying_price : option_contract -> unit
(** Functions to nicely print aspects of an option, except for steps which is
    hidden from user *)

val print_strike_price : option_contract -> unit
val print_time_to_expiry : option_contract -> unit
val print_volatility : option_contract -> unit
val print_interest_rate : option_contract -> unit
val print_option_type : option_contract -> unit
val print_option_contract : option_contract -> unit

val create_contract :
  float ->
  float ->
  float ->
  float ->
  float ->
  option_type ->
  int ->
  option_contract

val update_contract :
  option_contract -> float -> float -> float -> float -> option_contract
(** Updates the options contract based on factors that change over time so its
    current value can be found *)

val execute_contract : option_contract -> bool
(** Returns a boolean of whether the contract should be executed or not, true if
    it should and false if not*)

val cdf : float -> float
(** Represents the cumulative distribution function for a standard normal
    variable *)

val pdf : float -> float
(** Represents the probability distribution function for a standard normal
    variable *)

val black_scholes_price : option_contract -> float
(** Black-Scholes formula for European call option pricing *)

val delta : option_contract -> float
(** Greeks: Delta, Gamma, Theta, Vega, Rho *)

val gamma : option_contract -> float
val theta : option_contract -> float
val vega : option_contract -> float
val rho : option_contract -> float

val implied_volatility : option_contract -> float -> float
(** Implied volatility calculation using Newton's method *)

val binomial_price : option_contract -> float
(** Binomial method for call option pricing*)
