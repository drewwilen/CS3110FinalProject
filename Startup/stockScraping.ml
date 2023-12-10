open Yojson.Basic.Util

module StockScraping = struct
  type t = {
    open_price : float;
    high_price : float;
    low_price : float;
    close_price : float;
  }

  let base_url = "https://api.polygon.io"
  let api_key = "d_4M0nRmvVNVD7sPBita_pPrRNVMDuJx"

  let to_float_strict json =
    match json with
    | `Int i -> float_of_int i
    | `Float f -> f
    | _ -> failwith "Invalid JSON type for conversion to float"

  let get_ticker_info ticker =
    try
      let endpoint =
        "/v2/aggs/ticker/" ^ ticker ^ "/prev?adjusted=true&apiKey=" ^ api_key
      and query_params = [ ("apiKey", api_key) ] in
      let uri =
        Uri.with_query' (Uri.of_string (base_url ^ endpoint)) query_params
      in
      let url = Uri.to_string uri in
      let cmd = "curl -s -H 'Accept: application/json' '" ^ url ^ "'" in
      let ic = Unix.open_process_in cmd in
      let body_str = input_line ic in
      let body_json = Yojson.Basic.from_string body_str in

      let open_price =
        body_json |> member "results" |> to_list |> List.hd |> member "o"
        |> to_float_strict
      in
      let high_price =
        body_json |> member "results" |> to_list |> List.hd |> member "h"
        |> to_float_strict
      in
      let low_price =
        body_json |> member "results" |> to_list |> List.hd |> member "l"
        |> to_float_strict
      in
      let close_price =
        body_json |> member "results" |> to_list |> List.hd |> member "c"
        |> to_float_strict
      in

      { open_price; high_price; low_price; close_price }
    with _ ->
      print_endline "Invalid Stock";
      { open_price = 0.0; high_price = 0.0; low_price = 0.0; close_price = 0.0 }

  let to_string stock =
    "open price: "
    ^ string_of_float stock.open_price
    ^ "\nhigh price: "
    ^ string_of_float stock.high_price
    ^ "\nlow price: "
    ^ string_of_float stock.low_price
    ^ "\nclose price: "
    ^ string_of_float stock.close_price
    ^ "\n"

  let get_price stock = stock.close_price
end
