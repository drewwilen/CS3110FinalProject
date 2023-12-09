(* options_trading.ml *)

type option_type

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

(* Updates the options contract based on factors that change over time so its current value can be found *)
val updated_contract : option_contract -> float -> float -> float -> float -> option_contract

(* Represents the cumulative distribution function for a standard normal variable *)
val cdf : float -> float

(* Represents the probability distribution function for a standard normal variable *)
val pdf : float -> float

(* Black-Scholes formula for European call option pricing *)
val black_scholes_price : option_contract -> float

(* Greeks: Delta, Gamma, Theta, Vega *)
val delta : option_contract -> float
val gamma : option_contract -> float
val theta : option_contract -> float
val vega : option_contract -> float

(* Implied volatility calculation using Newton's method *)
val implied_volatility : option_contract -> float -> float

(* Binomial method for call option pricing*)
val binomial_price : option_contract -> float

