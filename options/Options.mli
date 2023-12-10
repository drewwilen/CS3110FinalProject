(* options_trading.ml *)

type option_type =
  | Call
  | Put

(* Define a record type for representing an options contract *)
type option_contract = {
  underlying_price : float;
  strike_price : float;
  time_to_expiry : float;
  volatility : float;
  interest_rate : float;
  option_type : option_type;
  steps : int;
}

(* Functions to nicely print aspects of an option, except for steps which is
   hidden from user *)
val print_underlying_price : option_contract -> unit
val print_strike_price : option_contract -> unit
val print_time_to_expiry : option_contract -> unit
val print_volatility : option_contract -> unit
val print_interest_rate : option_contract -> unit
val print_option_type : option_contract -> unit

(* Updates the options contract based on factors that change over time so its
   current value can be found *)
val update_contract :
  option_contract -> float -> float -> float -> float -> option_contract

(* Returns a boolean of whether the contract should be executed or not, true if
   it should and false if not*)
val execute_contract : option_contract -> bool

(* Represents the cumulative distribution function for a standard normal
   variable *)
val cdf : float -> float

(* Represents the probability distribution function for a standard normal
   variable *)
val pdf : float -> float

(* Black-Scholes formula for European call option pricing *)
val black_scholes_price : option_contract -> float

(* Greeks: Delta, Gamma, Theta, Vega, Rho *)
val delta : option_contract -> float
val gamma : option_contract -> float
val theta : option_contract -> float
val vega : option_contract -> float
val rho : option_contract -> float

(* Implied volatility calculation using Newton's method *)
val implied_volatility : option_contract -> float -> float

(* Binomial method for call option pricing*)
val binomial_price : option_contract -> float
