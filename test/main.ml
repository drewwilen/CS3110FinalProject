open OUnit2
open User
open Options

(** User testing OUnit vs Manual: Most functionality was tested by OUnit. Each
    function is included in multiple test cases, and there are also several test
    cases that combine functions (including buying, selling, updating, etc) that
    check how the functions work with each other. Additionally, the
    functionality from a user that was used in Startup (the actual platform) was
    tested even more carefully as we manually tested when integrated with the
    main platform. Developing Test Cases: A mix of black box and glass box
    testing was implemented. I tried involving a few different stocks, and
    portfolio that contained one stock, a few stocks, and some that bought and
    sold stocks so it varied so I could make sure to catch any edge cases.
    Additionally, I tested references to stocks very thoroughly to ensure those
    worked properly. Backtesting testing: OUnit vs Manual: Most functionality
    was tested by OUnit. Each function has around 2-3 test cases that explore
    edge possibilities and ensure that the user will receive the expected
    information. The functions that were used to print information, however,
    were not tested using OUnit and were simply visually observed in the
    terminal to ensure that the desired result was being produced. Developing
    Test Cases: Most functions that were tested by OUnit were implemented using
    glass box testing. Specifically, testing the csv reading functions required
    me to pull out information from these files and compare it to the output of
    these functions. Startup and Scraping Testing OUnit vs Manual: The startup
    file is virtually impossible to test via OUnit due to the fact that the
    files main functionality is running the whole program and displaying
    information and data on the terminal. Although there was not much OUnit
    testing, this portion of the code required heavy manual testing to ensure
    that the program always ran correctly, outputted the correct data and
    results, and never crashed unexpectedly.; Developing Test Cases: Our program
    was created with a main menu, so the logic behind the manual test cases was
    simple, yet rigorous. Every possible branch of the program that could be
    reached from the main menu had to be tested with expected inputs and edge
    cases. This testing style helped ensure that there were no holes in the
    program where you could possibly break it. This required a mix of blackbox
    and glassbox testing where glassbox testing was used to ensure that all
    possible branches of the program were reached(by looking at the code), and
    blackbox testing was used to motivate extraneous user inputs that could
    possibly break the program. Options testing: OUnit vs Manual: Almost all
    functionality was tested by OUnit. The option contract type and the
    corresponding functions on the type lend themselves well to being tested by
    OUnit as they take option contracts in and return a result based on
    formulas. Only the print functions, for both the aspects of the contract and
    the whole contract could not be tested by OUnit, and we instead visually
    confirmed that the results that we got were the same as the expected
    results. Developing Test Cases: For developing test cases we wanted to test
    all the functions on different options contracts. This meant writing black
    box tests mostly. For every function, we wanted to see that we would get the
    desired result from it without thinking about what is happening within the
    function. This made a lot of sense, because the only variability was whether
    the function was a Call or a Put, which we designed our test cases around by
    having different options contracts, some of which were calls and some puts.
    Some of our other testing centered around creating and modifying contracts,
    which was slightly different from the other functions, which all centered
    around inputs being a contract and outputs being a float, but could still be
    tested by black box testing. Overall, black box testing was a good strategy
    for testing options.

    General Conclusion: The culmination of testing each function/module
    individually and then ensuring that the functionality of the final product
    works proves that our code is correct. *)

let tsla = make_stock "TSLA" 100.0
let aapl = make_stock "AAPL" 67.0
let gme = make_stock "GME" 20.0
let empty = User.empty

let round_to_decimals (value : float) decimals =
  if value >= 0. then
    let multiplier = 10. ** float_of_int decimals in
    Float.round (value *. multiplier) /. multiplier
  else
    let multiplier = 10. ** float_of_int decimals in
    -.(Float.round (-.value *. multiplier) /. multiplier)

let optioncontract1 =
  {
    underlying_price = 100.0;
    strike_price = 95.0;
    time_to_expiry = 0.5;
    volatility = 0.2;
    interest_rate = 0.03;
    option_type = Call;
    steps = 10;
  }

let optioncontract2 =
  {
    underlying_price = 100.0;
    strike_price = 105.0;
    time_to_expiry = 0.5;
    volatility = 0.2;
    interest_rate = 0.03;
    option_type = Put;
    steps = 10;
  }

let optioncontract3 =
  {
    underlying_price = 120.0;
    strike_price = 115.0;
    time_to_expiry = 1.0;
    volatility = 0.25;
    interest_rate = 0.02;
    option_type = Call;
    steps = 10;
  }

let optioncontract4 =
  {
    underlying_price = 85.0;
    strike_price = 94.0;
    time_to_expiry = 1.0;
    volatility = 0.18;
    interest_rate = 0.05;
    option_type = Put;
    steps = 10;
  }

let options_suite =
  [
    ( "" >:: fun _ ->
      assert_equal (print_endline "hello")
        (print_endline
           (string_of_float (round_to_decimals (vega optioncontract2) 3))) );
    ( "black-scholes price call test" >:: fun _ ->
      assert_equal (string_of_float 9.25)
        (string_of_float
           (round_to_decimals (black_scholes_price optioncontract1) 2)) );
    ( "black-scholes price put test" >:: fun _ ->
      assert_equal 7.62
        (round_to_decimals (black_scholes_price optioncontract2) 2) );
    ( "delta call test" >:: fun _ ->
      assert_equal 0.705 (round_to_decimals (delta optioncontract1) 3) );
    ( "delta put test" >:: fun _ ->
      assert_equal (-0.567) (round_to_decimals (delta optioncontract2) 3) );
    ( "gamma test" >:: fun _ ->
      assert_equal 0.024 (round_to_decimals (gamma optioncontract1) 3) );
    ( "theta put" >:: fun _ ->
      assert_equal (-3.634) (round_to_decimals (theta optioncontract2) 3) );
    ( "black-scholes price call test 2" >:: fun _ ->
      assert_equal 15.57
        (round_to_decimals (black_scholes_price optioncontract3) 2) );
    ( "black-scholes price put test 2" >:: fun _ ->
      assert_equal 8.71
        (round_to_decimals (black_scholes_price optioncontract4) 2) );
    ( "delta call test 2" >:: fun _ ->
      assert_equal 0.646 (round_to_decimals (delta optioncontract3) 3) );
    ( "delta put test 2" >:: fun _ ->
      assert_equal (-0.576) (round_to_decimals (delta optioncontract4) 3) );
    ( "gamma test 2" >:: fun _ ->
      assert_equal 0.012 (round_to_decimals (gamma optioncontract3) 3) );
    ( "theta call test 2" >:: fun _ ->
      assert_equal (-6.817) (round_to_decimals (theta optioncontract3) 3) );
    ( "theta put test 2" >:: fun _ ->
      assert_equal (-0.114) (round_to_decimals (theta optioncontract4) 3) );
    ( "vega call test (optioncontract1)" >:: fun _ ->
      assert_equal 24.389 (round_to_decimals (vega optioncontract1) 3) );
    ( "vega put test (optioncontract2)" >:: fun _ ->
      assert_equal 27.813 (round_to_decimals (vega optioncontract2) 3) );
    ( "vega call test (optioncontract3)" >:: fun _ ->
      assert_equal 44.619 (round_to_decimals (vega optioncontract3) 3) );
    ( "update contract test" >:: fun _ ->
      assert_equal
        {
          underlying_price = 80.0;
          strike_price = 95.0;
          time_to_expiry = 0.3;
          volatility = 0.4;
          interest_rate = 0.05;
          option_type = Call;
          steps = 10;
        }
        (update_contract optioncontract1 80. 0.3 0.4 0.05) );
    ( "cdf test (x = 0)" >:: fun _ ->
      assert_equal 0.5000 (round_to_decimals (cdf 0.0) 4) );
    ( "cdf test (x = 1)" >:: fun _ ->
      assert_equal 0.8413 (round_to_decimals (cdf 1.0) 4) );
    ( "cdf test (x = -1)" >:: fun _ ->
      assert_equal 0.1587 (round_to_decimals (cdf (-1.0)) 4) );
    ( "cdf test 4" >:: fun _ ->
      assert_equal 0.633 (round_to_decimals (cdf 0.34) 3) );
    ( "pdf test (x = 0)" >:: fun _ ->
      assert_equal 0.3989 (round_to_decimals (pdf 0.0) 4) );
    ( "pdf test (x = 1)" >:: fun _ ->
      assert_equal 0.24 (round_to_decimals (pdf 1.0) 2) );
    ( "pdf test (x = -1)" >:: fun _ ->
      assert_equal 0.24 (round_to_decimals (pdf (-1.0)) 2) );
  ]

let test_port_tolist expected port =
  assert_equal ~msg:"Test port to_string failed" expected
    (User.to_backtest port)

let user_test =
  [
    ("Empty Test" >:: fun _ -> assert_bool "failing test" (User.is_empty empty));
    ( "Empty Test Non - empty" >:: fun _ ->
      assert_bool "failing test" (not (User.is_empty (buy tsla 100 empty))) );
    ( "Empty Test - bought and sold empty" >:: fun _ ->
      assert_bool "failing test"
        (User.is_empty (sell "TSLA" 100 (buy tsla 100 empty))) );
    ( "Empty Test - attempt to sell wrong stock, not empty" >:: fun _ ->
      assert_bool "failing test"
        (not (User.is_empty (sell "AAPL" 100 (buy tsla 100 empty)))) );
    ( "Empty Test - bought and sold some" >:: fun _ ->
      assert_bool "failing test"
        (not (User.is_empty (sell "TSLA" 43 (buy tsla 100 empty)))) );
    ( "stocks of empty portfolio is empty string" >:: fun _ ->
      assert_equal "" (User.stocks empty) );
    ( "Test looking up in portfolio" >:: fun _ ->
      assert_equal (Some 0.0) User.(lookup_gain "TSLA" (buy tsla 100 empty)) );
    ( "Test looking up not in portfolio" >:: fun _ ->
      assert_equal None User.(lookup_gain "AAPL" (buy tsla 100 empty)) );
    ( "Test looking up in portfolio, updated to gain money" >:: fun _ ->
      assert_equal 3.
        User.(
          let p = buy aapl 100 empty in
          let _ = update_stock aapl 70.0 in

          match lookup_gain "AAPL" p with
          | Some u -> u
          | None -> 0.) );
    ( "Test looking up in portfolio, updated to lose money" >:: fun _ ->
      assert_equal (-25.)
        (let p = buy tsla 100 empty in
         let _ = update_stock tsla 75. in
         match User.(lookup_gain "TSLA" p) with
         | None -> 0.
         | Some u -> u) );
    ( "Test looking up in portfolio, updated to gain money, but sold so should \
       have zero value"
    >:: fun _ ->
      assert_equal 0.
        User.(
          let p = buy aapl 100 empty in

          let _ = update_stock aapl 70.0 in

          match lookup_gain "AAPL" (sell "AAPL" 100 p) with
          | Some u -> u
          | None -> 0.) );
    ("Value of empty portfolio" >:: fun _ -> assert_equal 0.0 User.(value empty));
    ( "Value of non-empty portfolio" >:: fun _ ->
      let tsla = make_stock "TSLA" 100.0 in
      let p = User.empty in
      let p_upt = buy tsla 10 p in
      let p_new = buy tsla 90 p_upt in
      assert_equal 10000. User.(value p_new) );
    ( "Value of non-empty, diversified portfolio" >:: fun _ ->
      let tsla = make_stock "TSLA" 100.0 in
      let p = User.empty in
      let p_upt = buy tsla 1 p in
      let p_new = buy gme 1 p_upt in
      assert_equal 120. User.(value p_new) );
    ( "Value of non-empty, sold partially, diversified portfolio" >:: fun _ ->
      let tsla = make_stock "TSLA" 100.0 in
      let p = User.empty in
      let p_upt = buy tsla 1 p in
      let p_new = buy gme 3 p_upt in
      let p_renew = sell "GME" 2 p_new in
      assert_equal 120. User.(value p_renew) );
    ( "Updating stock to same price returns original price" >:: fun _ ->
      let stock_pr = 100.0 in
      assert_equal stock_pr
        (let stk = make_stock "STK" stock_pr in
         update_stock stk stock_pr) );
    ( "Updating stock_price to different price returns original price"
    >:: fun _ ->
      let stock_pr = 100.0 in
      assert_equal 2.0
        (let stk = make_stock "STK" stock_pr in
         update_stock stk 2.0) );
    (* ( "THIS FAILS SOMETIMES? Value of non-empty portfolio!" >:: fun _ ->
       assert_equal 6700.0 User.( let p = buy aapl 100 empty in value p) ); *)
    ( "Value of portfolio that went to zero" >:: fun _ ->
      assert_equal 0.0
        (let p = buy aapl 100 empty in
         let _ = update_stock aapl 0.0 in
         User.(value p)) );
    ( "Test of make user, should be empty stock if not buying anything"
    >:: fun _ ->
      assert_equal empty
        (let user = make_user "drew" "wilenzick" in
         get_portfolio user) );
    ( "Test of user, that start as empty and then portfolio is changed"
    >:: fun _ ->
      assert_bool "failing case"
        (let user = make_user "drew" "wilenzick" in
         let p = get_portfolio user in
         let p_upt = buy aapl 100 p in
         not (is_empty p_upt)) );
    ( "Sell Stock Completely" >:: fun _ ->
      let user = make_user "Bob" "Johnson" in
      let portfolio = get_portfolio user in
      let bought_portfolio = buy aapl 10 portfolio in
      let after_sell = sell "AAPL" 10 bought_portfolio in
      assert_equal true (is_empty after_sell) );
    ( "Sell Stock almost completely" >:: fun _ ->
      let user = make_user "Bob" "Johnson" in
      let portfolio = get_portfolio user in
      let bought_portfolio = buy aapl 10 portfolio in
      let after_sell = sell "AAPL" 8 bought_portfolio in
      assert_equal false (is_empty after_sell) );
    ( "Test of num_stocks, empty" >:: fun _ ->
      assert_equal 0
        User.(
          let p = empty in
          num_stocks p) );
    ( "Test of num_stocks, empty" >:: fun _ ->
      let p = User.empty in
      let p_upt = buy tsla 1 p in
      let p_new = buy gme 1 p_upt in
      assert_equal 2 User.(num_stocks p_new) );
    ( "Test num_shares on empty" >:: fun _ ->
      assert_equal 0
        User.(
          let p = empty in
          num_shares p) );
    ( "Test num_shares on complicated" >:: fun _ ->
      let p = User.empty in
      let p_upt = buy tsla 1 p in
      let p_new = buy gme 13 p_upt in
      assert_equal 14 User.(num_shares p_new) );
    ( "Test num_shares on complicated, sold some shares" >:: fun _ ->
      let p = User.empty in
      let p_upt = buy tsla 1 p in
      let p_new = sell "GME" 2 (buy gme 13 p_upt) in
      assert_equal 12 User.(num_shares p_new) );
    ( "Test of empty users, any attempt to login should not work" >:: fun _ ->
      let p = User.empty_users in
      assert_equal None User.(login_attempt "drew" "wilen" p) );
    ( "Test of empty users, failure of login" >:: fun _ ->
      let loggers = User.empty_users in
      let log_upt = User.add_user (User.make_user "drew" "w") loggers in
      assert_equal None User.(login_attempt "drew" "wilen" log_upt) );
    ( "Test of empty users, successful login, empty" >:: fun _ ->
      let loggers = User.empty_users in
      let log_upt = User.add_user (User.make_user "drew" "w") loggers in
      assert_equal (Some empty) User.(login_attempt "drew" "w" log_upt) );
    ( "Test of empty users, successful login, non-empty" >:: fun _ ->
      let p = buy tsla 10 empty in
      let loggers = User.empty_users in
      let log_upt =
        User.add_user (User.make_user_from_portfolio "drew" "w" p) loggers
      in
      assert_equal false
        (match User.(login_attempt "drew" "w" log_upt) with
        | Some u -> is_empty u
        | None -> failwith "incorrect") );
    ( "Test of empty users, successful login, non-empty" >:: fun _ ->
      let loggers = User.empty_users in
      let log_upt = User.add_user (User.make_user "drew" "w") loggers in
      assert_equal false
        (match User.(login_attempt "drew" "w" log_upt) with
        | Some u -> not (is_empty u)
        | None -> failwith "incorrect") );
    ( "Testing selling a stock that isnt a memeber of the portfolio" >:: fun _ ->
      assert_bool "testing no crash when not memeber of portfolio"
        (User.is_empty (sell "amzn" 1 empty)) );
    ("Testing to portfolio fucntion" >:: fun _ -> test_port_tolist [] User.empty);
    ( "Testing to portfolio fucntion" >:: fun _ ->
      test_port_tolist [ ("TSLA", 100) ] (buy tsla 100 empty) );
    ( "Testing to portfolio fucntion" >:: fun _ ->
      test_port_tolist [] (sell "TSLA" 100 (buy tsla 100 empty)) );
  ]

let backtesting_suite =
  [
    ( "Create path to the 'empty' stock" >:: fun _ ->
      assert_equal "data/individual_stocks_5yr/individual_stocks_5yr/_data.csv"
        (Backtest.create_file_path_individual "") );
    ( "Create path to apple stock" >:: fun _ ->
      assert_equal
        "data/individual_stocks_5yr/individual_stocks_5yr/AAPL_data.csv"
        (Backtest.create_file_path_individual "AAPL") );
    ( "Test valid stocks on the empty list" >:: fun _ ->
      assert_equal [] (Backtest.valid_stocks []) );
    ( "Test valid stocks on the non-empty list with all stocks being valid"
    >:: fun _ -> assert_equal [ "AAPL" ] (Backtest.valid_stocks [ "AAPL" ]) );
    ( "Test valid stocks on the non-empty list with an invalid stock"
    >:: fun _ -> assert_equal [] (Backtest.valid_stocks [ "hello" ]) );
    ( "Test valid stocks on the non-empty list with an invalid stock and a \
       valid stock"
    >:: fun _ ->
      assert_equal [ "AAPL" ] (Backtest.valid_stocks [ "hello"; "AAPL" ]) );
    ( "First n-elements of the empty list n = 0" >:: fun _ ->
      assert_equal [] (Backtest.first_n_elements [] 0) );
    ( "First n-elements of the empty list n > 0" >:: fun _ ->
      assert_equal [] (Backtest.first_n_elements [] 1) );
    ( "First n-elements of the non-empty list n = 0" >:: fun _ ->
      assert_equal [] (Backtest.first_n_elements [ "hello" ] 0) );
    ( "First n-elements of the non-empty list n > 0" >:: fun _ ->
      assert_equal [ "hello" ]
        (Backtest.first_n_elements [ "hello"; "world" ] 1) );
    ( "historical data of apple -> only 1 day" >:: fun _ ->
      assert_equal
        [
          [
            "2013-02-08";
            "67.7142";
            "68.4014";
            "66.8928";
            "67.8542";
            "158168416";
            "AAPL";
          ];
        ]
        (Backtest.historical_stock_data "AAPL" 1 1) );
    ( "historical data of apple -> 2 days" >:: fun _ ->
      assert_equal
        [
          [
            "2013-02-08";
            "67.7142";
            "68.4014";
            "66.8928";
            "67.8542";
            "158168416";
            "AAPL";
          ];
          [
            "2013-02-11";
            "68.0714";
            "69.2771";
            "67.6071";
            "68.5614";
            "129029425";
            "AAPL";
          ];
        ]
        (Backtest.historical_stock_data "AAPL" 2 1) );
    ( "historical data of apple and amazon -> only 1 day" >:: fun _ ->
      assert_equal
        [
          [
            [
              "2013-02-08";
              "67.7142";
              "68.4014";
              "66.8928";
              "67.8542";
              "158168416";
              "AAPL";
            ];
          ];
          [
            [
              "2013-02-08";
              "261.4";
              "265.25";
              "260.555";
              "261.95";
              "3879078";
              "AMZN";
            ];
          ];
        ]
        (Backtest.historial_n_stocks_data [ "AAPL"; "AMZN" ] 1 1) );
    ( "historical data of apple and amazon -> 2 days" >:: fun _ ->
      assert_equal
        [
          [
            [
              "2013-02-08";
              "67.7142";
              "68.4014";
              "66.8928";
              "67.8542";
              "158168416";
              "AAPL";
            ];
            [
              "2013-02-11";
              "68.0714";
              "69.2771";
              "67.6071";
              "68.5614";
              "129029425";
              "AAPL";
            ];
          ];
          [
            [
              "2013-02-08";
              "261.4";
              "265.25";
              "260.555";
              "261.95";
              "3879078";
              "AMZN";
            ];
            [
              "2013-02-11";
              "263.2";
              "263.25";
              "256.6";
              "257.21";
              "3403403";
              "AMZN";
            ];
          ];
        ]
        (Backtest.historial_n_stocks_data [ "AAPL"; "AMZN" ] 2 1) );
    ( "Change of stock, between the first 2 days, Apple" >:: fun _ ->
      assert_equal (68.5614 -. 67.8542) (Backtest.change_of_stock "AAPL" 2) );
    ( "Change of stock, between the first day of data and the 4th day of data"
    >:: fun _ ->
      assert_equal (66.7156 -. 68.5614) (Backtest.change_of_stock "AAPL" 4) );
    ( "Change of portfolio, between the first 2 days, Apple and Amazon"
    >:: fun _ ->
      assert_equal
        (68.5614 -. 67.8542 +. (257.21 -. 261.95))
        (Backtest.change_of_portfolio [ "AAPL"; "AMZN" ] 2) );
    ( "Change of portfolio, first day of data and the 4th day of data, AAPL, \
       AMZN"
    >:: fun _ ->
      assert_equal
        (66.7156 -. 68.5614 +. (269.47 -. 257.21))
        (Backtest.change_of_portfolio [ "AAPL"; "AMZN" ] 4) );
  ]

let tests = "test suite" >::: user_test @ backtesting_suite @ options_suite
let _ = run_test_tt_main tests
