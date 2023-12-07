type key = string

type stock = {
  ticker : key;
  bought : float;
}

type value = {
  stock : stock;
  shares : int;
  unrealized_gain : float;
  percentage_gain : float;
  dividend : float option;
}

type t = (key * value) list

let empty = []
let is_empty t = List.length t = 0
let stocks _ = failwith "Unimplemented"

let stock_to_string (s : value) (i : int) =
  "Portfolio Stock # " ^ string_of_int i ^ "Ticker " ^ s.stock.ticker ^ "\n"
  ^ "Bought at Price: "
  ^ string_of_float s.stock.bought
  ^ "\n" ^ "Number of shares bought: " ^ string_of_int s.shares ^ "\n"
  ^ "Unrealized gain since purchase: "
  ^ string_of_float s.unrealized_gain
  ^ "Percentage gain since purchase: "
  ^ string_of_float s.percentage_gain
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

(* let portfolio = failwith "unimplemented" *)
let lookup _ _ = failwith "Unimplemented"
let value _ = failwith "Unimplemented"
let make_stock ticker bought = { ticker; bought }

let buy stock num portfolio =
  ( stock.ticker,
    {
      stock;
      shares = num;
      unrealized_gain = 0.0;
      percentage_gain = 0.0;
      dividend = None;
    } )
  :: portfolio

let sell _ _ _ = failwith "Unimplemented"
let update _ = failwith "Unimplemented"
