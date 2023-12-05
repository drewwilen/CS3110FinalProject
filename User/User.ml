type key = string

type value = {
  ticker : key;
  bought : float;
  shares : int;
  unrealized_gain : float;
  percentage_gain : float;
  dividend : float option;
}

type t = (key * value) list

let empty = []
let portfolio = failwith "unimplemented"
let is_empty t = List.length t = 0
let stocks t = failwith "Unimplemented"
let print_portfolio t = failwith "Unimplemented"
let lookup k t = failwith "Unimplemented"
let value t = failwith "Unimplemented"
let buy k n t = failwith "Unimplemented"
let sell k n t = failwith "Unimplemented"
let update t = failwith "Unimplemented"
