(* options_trading.ml *)


(* Option type representing call or put *)
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
  steps : int; (* Number of steps in the binomial model *)
}

let erf x =
  let a1 = 0.254829592 in
  let a2 = -0.284496736 in
  let a3 = 1.421413741 in
  let a4 = -1.453152027 in
  let a5 = 1.061405429 in
  let p = 0.3275911 in
  let sign = if x < 0.0 then -1.0 else 1.0 in
  let x = abs_float x in
  let t = 1.0 /. (1.0 +. p *. x) in
  let y = 1.0 -. (((((a5 *. t +. a4) *. t) +. a3) *. t +. a2) *. t +. a1) *. t *. exp (-. x *. x) in
  sign *. y;;

let print_underlying_price (contract : option_contract) =
  print_endline
    ("The underlying price is: " ^ string_of_float contract.underlying_price)

let print_strike_price (contract : option_contract) =
  print_endline ("The strike price is: " ^ string_of_float contract.strike_price)

let print_time_to_expiry (contract : option_contract) =
  print_endline
    ("The time to expiry is: " ^ string_of_float contract.time_to_expiry)

let print_volatility (contract : option_contract) =
  print_endline ("The volatility is: " ^ string_of_float contract.time_to_expiry)

let print_interest_rate (contract : option_contract) =
  print_endline
    ("The interest_rate is: " ^ string_of_float contract.time_to_expiry)

let print_option_type (contract : option_contract) =
  match contract.option_type with
  | Call -> print_endline "Call"
  | Put -> print_endline "Put"

let update_contract (contract : option_contract) (updated_price : float)
    (updated_time : float) (updated_volatility : float)
    (updated_interest_rate : float) =
  {
    underlying_price = updated_price;
    strike_price = contract.strike_price;
    time_to_expiry = updated_time;
    volatility = updated_volatility;
    interest_rate = updated_interest_rate;
    option_type = contract.option_type;
    steps = contract.steps;
  }

let execute_contract (contract : option_contract) =
  match contract.option_type with
    | Call -> if contract.time_to_expiry<=0. then contract.underlying_price<=contract.strike_price else false
    | Put -> if contract.time_to_expiry<=0. then contract.underlying_price>=contract.strike_price else false

(* Cumulative distribution function for the standard normal distribution *)
let cdf (x : float) : float =
  (*1. /. (1. +. 2.*. exp(-.sqrt(2. *. Float.pi)*.x))*)
  (1. +. erf (x/.(sqrt 2.)))/.2.

(* Probability density function for the standard normal distribution *)
let pdf (x : float) : float =
  let a = 1.0 /. sqrt (2.0 *. Float.pi) in
  a *. exp (-0.5 *. x *. x)

(* Black-Scholes formula for European option pricing *)
let black_scholes_price (contract : option_contract) : float =
  let s = contract.underlying_price in
  let k = contract.strike_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let r = contract.interest_rate in

  let d1 = (log (s /. k) +. ((r +. (v *. v /. 2.0)) *. t)) /. (v *. sqrt t) in
  let d2 = d1 -. (v *. sqrt t) in

  match contract.option_type with
    | Call -> (s*. (cdf d1)) -. (k*.exp(-.r*.t)*.(cdf d2))
    | Put -> (k*.exp(-.r*.t)*.(cdf (-.d2))) -. (s*. (cdf (-.d1)))


(* Greeks: Delta, Gamma, Theta, Vega *)
let delta (contract : option_contract) : float =
  let s = contract.underlying_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let d1 =
    (log (s /. contract.strike_price)
    +. ((contract.interest_rate +. (v *. v /. 2.0)) *. t))
    /. (v *. sqrt t)
  in
  match contract.option_type with
  | Call -> cdf d1
  | Put -> -1.0 *. cdf (-.d1)

let gamma (contract : option_contract) : float =
  let s = contract.underlying_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let d1 =
    (log (s /. contract.strike_price)
    +. ((contract.interest_rate +. (v *. v /. 2.0)) *. t))
    /. (v *. sqrt t)
  in
  pdf d1 /. (s *. v *. sqrt t)

let rho (contract : option_contract) : float =
  let s = contract.underlying_price in
  let k = contract.strike_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let r = contract.interest_rate in

  let d1 = (log (s /. k) +. ((r +. (v *. v /. 2.0)) *. t)) /. (v *. sqrt t) in
  let d2 = d1 -. (v *. sqrt t) in
  match contract.option_type with
    | Call -> k*.t*. exp(-.r*.t) *. cdf (d2)
    | Put -> -.k*.t*. exp(-.r*.t) *. cdf (d2)

let theta (contract : option_contract) : float =
  let s = contract.underlying_price in
  let k = contract.strike_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let r = contract.interest_rate in

  let d1 = (log (s /. k) +. ((r +. (v *. v /. 2.0)) *. t)) /. (v *. sqrt t) in
  let d2 = d1 -. (v *. sqrt t) in
  match contract.option_type with
    | Call -> -.((s*.(pdf d1)*.v)/.(2.*.(sqrt(t)))) -. (r*.k*.exp(-.r*.t)*. (cdf d2))
    | Put -> -.(s*.(pdf d1)*.v)/.(2.*.(sqrt(t))) +. (r*.k*.exp(-.r*.t)*. (cdf (-.d2)))

let vega (contract : option_contract) : float =
  let s = contract.underlying_price in
  let t = contract.time_to_expiry in
  let v = contract.volatility in
  let d1 =
    (log (s /. contract.strike_price)
    +. ((contract.interest_rate +. (v *. v /. 2.0)) *. t))
    /. (v *. sqrt t)
  in
  s *. sqrt t *. pdf d1

(* Implied volatility calculation using Newton's method *)
let implied_volatility (contract : option_contract) (target_price : float) :
    float =
  let tolerance = 1e-6 in
  let max_iterations = 100 in
  let rec find_volatility (volatility : float) (iteration : int) : float =
    let price = black_scholes_price { contract with volatility } in
    let vega_value = vega { contract with volatility } in
    let new_volatility =
      volatility -. ((price -. target_price) /. vega_value)
    in

    if
      abs_float (price -. target_price) < tolerance
      || iteration >= max_iterations
    then new_volatility
    else find_volatility new_volatility (iteration + 1)
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
  let dt = t /. float_of_int steps in
  let u = exp (v *. sqrt dt) in
  let d = 1.0 /. u in
  let p = (exp (r *. dt) -. d) /. (u -. d) in

  let rec binomial_recursive (n : int) (m : int) : float =
    if n = steps then
      max
        (match contract.option_type with
        | Call ->
            (s *. (u ** float_of_int m) *. (d ** float_of_int (steps - m))) -. k
        | Put ->
            k -. (s *. (u ** float_of_int m) *. (d ** float_of_int (steps - m))))
        0.0
    else
      let up_price = binomial_recursive (n + 1) (m + 1) in
      let down_price = binomial_recursive (n + 1) m in
      let discounted_value = exp (-.r *. dt) in
      let option_value =
        ((p *. up_price) +. ((1.0 -. p) *. down_price)) *. discounted_value
      in
      max option_value 0.0
  in

  binomial_recursive 0 0
