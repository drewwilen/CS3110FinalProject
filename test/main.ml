open OUnit2
open User

let tsla = make_stock "TSLA" 100.0

let suite =
  "scraping test suite"
  >::: [
         ( "Empty Test" >:: fun _ ->
           assert_bool "failing test" (User.is_empty empty) );
         ( "Empty Test Non - empty" >:: fun _ ->
           assert_bool "failing test" (not (User.is_empty (buy tsla 100 empty)))
         );
       ]

let _ = run_test_tt_main suite
