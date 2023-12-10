type key = string

type stock = {
  ticker : key;
  price : float ref;
}

type value = {
  stock : stock;
  bought_price : float;
  shares : int ref;
  unrealized_gain : float ref;
  percentage_gain : float ref;
  dividend : float option;
}

type login = string * string
type t = (key * value) list
type user = login * t
type users = user list

let empty = []
let is_empty t = List.length t = 0

let make_user (username : string) (password : string) =
  ((username, password), empty)

let make_user_from_portfolio (username : string) (password : string) portfolio =
  ((username, password), portfolio)

let get_portfolio (user : user) = snd user

let stock_to_string (s : value) (i : int) =
  "\nPortfolio Stock # " ^ string_of_int i ^ "\nTicker " ^ s.stock.ticker ^ "s"
  ^ "\nBought at Price: "
  ^ string_of_float s.bought_price
  ^ "\n" ^ "Number of shares bought: " ^ string_of_int !(s.shares) ^ "\n"
  ^ "Unrealized gain since purchase: "
  ^ string_of_float !(s.unrealized_gain)
  ^ "\nPercentage gain since purchase: "
  ^ string_of_float !(s.percentage_gain)
  ^ "\n"
  ^
  match s.dividend with
  | Some d -> "Dividend value: " ^ string_of_float d
  | None -> ""

let rec to_string_stock_list lst accum =
  match lst with
  | [] -> ""
  | (_, v) :: t -> stock_to_string v accum ^ to_string_stock_list t (accum + 1)

let print_portfolio p = print_string (to_string_stock_list p 1)
let stocks t = to_string_stock_list t 1

let rec num_stocks p =
  match p with
  | [] -> 0
  | _ :: t -> 1 + num_stocks t

let rec num_shares p =
  match p with
  | [] -> 0
  | (_, v) :: t -> !(v.shares) + num_shares t

let rec value portfolio =
  match portfolio with
  | [] -> 0.0
  | (_, h) :: t -> (float_of_int !(h.shares) *. !(h.stock.price)) +. value t

let make_stock ticker bought = { ticker; price = ref bought }

let buy stock num portfolio =
  let price = !(stock.price) in
  ( stock.ticker,
    {
      stock;
      bought_price = price;
      shares = ref num;
      unrealized_gain = ref 0.0;
      percentage_gain = ref 0.0;
      dividend = None;
    } )
  :: portfolio

let sell stock num portfolio =
  match List.assoc_opt stock.ticker portfolio with
  | Some bought_stock ->
      let curr_shares = !(bought_stock.shares) in
      if curr_shares <= num then List.remove_assoc stock.ticker portfolio
      else (
        bought_stock.shares := curr_shares - num;
        portfolio)
  | None -> portfolio

let update_stock stock price =
  stock.price := price;
  price

let rec update portfolio =
  match portfolio with
  | [] -> []
  | (k, v) :: t ->
      v.unrealized_gain := !(v.stock.price) -. v.bought_price;
      v.percentage_gain := !(v.stock.price) /. v.bought_price;
      (k, v) :: update t

let lookup_gain (ticker : string) (portfolio : t) =
  let updated_portfolio = update portfolio in
  match List.assoc_opt ticker updated_portfolio with
  | Some stock -> Some !(stock.unrealized_gain)
  | None -> None

let rec to_backtest portfolio =
  match portfolio with
  | [] -> []
  | (ticker, stock) :: t -> (ticker, !(stock.shares)) :: to_backtest t
