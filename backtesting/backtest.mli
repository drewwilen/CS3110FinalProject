(* backtesting.mli *)
val read_csv_file : string -> string list list
val s_and_p : string list
val create_file_path_individual : string -> string
val create_list : string -> string list list
val print_data : string list list -> unit
val valid_stocks : string list -> string list
val first_n_elements : 'a list -> int -> 'a list
val historical_stock_data : string -> int -> int -> string list list
val historial_n_stocks_data : string list -> int -> int -> string list list list
val change_of_stock : string -> int -> float
val change_of_portfolio : string list -> int -> float
val change_of_portfolio_adjust : (string * int) list -> int -> float
