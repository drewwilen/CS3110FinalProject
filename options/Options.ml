(* options_trading.ml *)

open Owl

(* Option type representing call or put *)
type option_type = Call | Put

(* Define a record type for representing an options contract *)
type option_contract = {
  underlying_price : float;
  strike_price : float;
  time_to_expiry : float;
  volatility : float;
  interest_rate : float;
  option_type : option_type;
  steps : int;  (* Number of steps in the binomial model *)
}

(* Black-Scholes formula for European option pricing *)
let black_scholes_price (contract : option_contract) : float =
  let s = contract.underlying_price in
  let k = contract.strike_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let r = contract.interest_rate in
  let option_type_multiplier = match contract.option_type with
    | Call -> 1.0
    | Put -> -1.0
  in

  let d1 = (log(s /. k) +. (r +. (v *. v) /. 2.0) *. t) /.
           (v *. sqrt t) in
  let d2 = d1 -. (v *. sqrt t) in

  option_type_multiplier *. (s *. Owl.gaussian_cdf d1 -. k *. exp(-.r *. t) *. Owl.gaussian_cdf d2)

(* Greeks: Delta, Gamma, Theta, Vega *)
let delta (contract : option_contract) : float =
  let s = contract.underlying_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let d1 = (log(s /. contract.strike_price) +. (contract.interest_rate +. (v *. v) /. 2.0) *. t) /.
           (v *. sqrt t) in
  match contract.option_type with
  | Call -> Owl.gaussian_cdf d1
  | Put -> -1.0 *. Owl.gaussian_cdf (-.d1)

let gamma (contract : option_contract) : float =
  let s = contract.underlying_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let d1 = (log(s /. contract.strike_price) +. (contract.interest_rate +. (v *. v) /. 2.0) *. t) /.
           (v *. sqrt t) in
  Owl.gaussian_pdf d1 / (s *. v *. sqrt t)

let theta (contract : option_contract) : float =
  let s = contract.underlying_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let r = contract.interest_rate in
  let d1 = (log(s /. contract.strike_price) +. (r +. (v *. v) /. 2.0) *. t) /.
           (v *. sqrt t) in
  let d2 = d1 -. (v *. sqrt t) in
  let option_type_multiplier = match contract.option_type with
    | Call -> 1.0
    | Put -> -1.0
  in
  option_type_multiplier *. (-s *. Owl.gaussian_pdf d1 *. v / (2.0 *. sqrt t) +
                             r *. contract.strike_price *. exp(-.r *. t) *. Owl.gaussian_cdf (option_type_multiplier *. d2))

let vega (contract : option_contract) : float =
  let s = contract.underlying_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let d1 = (log(s /. contract.strike_price) +. (contract.interest_rate +. (v *. v) /. 2.0) *. t) /.
           (v *. sqrt t) in
  s *. sqrt t *. Owl.gaussian_pdf d1

(* Implied volatility calculation using Newton's method *)
let implied_volatility (contract : option_contract) (target_price : float) : float =
  let tolerance = 1e-6 in
  let max_iterations = 100 in
  let rec find_volatility (volatility : float) (iteration : int) : float =
    let price = black_scholes_price {contract with volatility} in
    let vega_value = vega {contract with volatility} in
    let new_volatility = volatility -. (price -. target_price) /. vega_value in

    if abs_float(price -. target_price) < tolerance || iteration >= max_iterations then
      new_volatility
    else
      find_volatility new_volatility (iteration + 1)
  in

  find_volatility contract.volatility 0

(* Binomial pricing model for European options *)
let binomial_price (contract : option_contract) : float =
  let s = contract.underlying_price in
  let k = contract.strike_price in
  let t = contract.time_to_expiry in
  let r = contract.interest_rate in
  let v = contract.volatility in
  let steps = contract.steps in
  let dt = t /. (float_of_int steps) in
  let u = exp (v *. sqrt dt) in
  let d = 1.0 /. u in
  let p = (exp (r *. dt) -. d) /. (u -. d) in

  let rec binomial_recursive (n : int) (m : int) : float =
    if n = steps then
      max (match contract.option_type with
        | Call -> s *. (u ** float_of_int m) *. (d ** float_of_int (steps - m)) -. k
        | Put -> k -. s *. (u ** float_of_int m) *. (d ** float_of_int (steps - m))) 0.0
    else
      let up_price = binomial_recursive (n + 1) (m + 1) in
      let down_price = binomial_recursive (n + 1) m in
      let discounted_value = exp (-.r *. dt) in
      let option_value = (p *. up_price +. (1.0 -. p) *. down_price) *. discounted_value in
      max option_value 0.0
  in

  binomial_recursive 0 0