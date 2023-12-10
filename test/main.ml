open OUnit2
open User

let tsla = make_stock "TSLA" 100.0
let tsla_updated = make_stock "TSLA" 75.0
let tsla_zero = make_stock "TSLA" 75.0
let aapl = make_stock "AAPL" 67.0
let aapl_updated = make_stock "AAPL" 75.0

let suite =
  "scraping test suite"
  >::: [
         ( "Empty Test" >:: fun _ ->
           assert_bool "failing test" (User.is_empty empty) );
         ( "Empty Test Non - empty" >:: fun _ ->
           assert_bool "failing test" (not (User.is_empty (buy tsla 100 empty)))
         );
         ( "Empty Test - bought and sold empty" >:: fun _ ->
           assert_bool "failing test"
             (User.is_empty (sell tsla 100 (buy tsla 100 empty))) );
         ( "Empty Test - attempt to sell wrong stock, not empty" >:: fun _ ->
           assert_bool "failing test"
             (not (User.is_empty (sell aapl 100 (buy tsla 100 empty)))) );
         ( "Empty Test - bought and sold some" >:: fun _ ->
           assert_bool "failing test"
             (not (User.is_empty (sell tsla 43 (buy tsla 100 empty)))) );
         ( "stocks of empty portfolio is empty string" >:: fun _ ->
           assert_equal "" (User.stocks empty) );
         ( "Test looking up in portfolio" >:: fun _ ->
           assert_equal (Some 0.0)
             User.(lookup_gain "TSLA" (buy tsla 100 empty)) );
         ( "Test looking up not in portfolio" >:: fun _ ->
           assert_equal None User.(lookup_gain "AAPL" (buy tsla 100 empty)) );
         ( "Test looking up in portfolio, updated to gain money" >:: fun _ ->
           assert_equal (Some 8.0)
             User.(
               lookup_gain "AAPL" (update aapl_updated (buy aapl 100 empty))) );
         ( "Test looking up in portfolio, updated to gain money" >:: fun _ ->
           assert_equal (Some (-25.0))
             User.(
               lookup_gain "TSLA" (update tsla_updated (buy tsla 100 empty))) );
         ( "Value of empty portfolio" >:: fun _ ->
           assert_equal 0.0 User.(value (update tsla_updated empty)) );
         ( "Value of non-empty portfolio" >:: fun _ ->
           assert_equal 6700.0
             User.(
               print_float (value (buy aapl 100 empty));
               value (buy aapl 100 empty)) );
         ( "Value of portfolio that went to zero" >:: fun _ ->
           assert_equal 0.0 User.(value (update tsla_zero (buy tsla 100 empty)))
         );
         ( "Value of portfolio that went to zero" >:: fun _ ->
           assert_equal [ "AAPL" ] (Backtest.valid_stocks [ "AAPL"; "poop" ])
         )
         (* Test for non-empty portfolio *);
       ]

let _ = run_test_tt_main suite
