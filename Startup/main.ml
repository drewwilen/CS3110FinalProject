open StockScraping

(**function to make lines bold in the terminal and read lines*)
let read_line_with_prompt prompt =
  ANSITerminal.(print_string [ Bold ] prompt);
  flush stdout;
  input_line stdin

(**Method to open and read file*)
let file_reader filename =
  let open_file = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line open_file in
      read_lines (line :: acc)
    with End_of_file ->
      close_in open_file;
      List.rev acc
  in
  read_lines []

(**function to check valid date format*)
let is_valid_date_format s =
  let parse_month str index =
    if index + 2 <= String.length str then
      let month_str = String.sub str index 2 in
      try
        let month = int_of_string month_str in
        month >= 1 && month <= 12
      with _ -> false
    else false
  in

  let parse_day str index =
    if index + 2 <= String.length str then
      let day_str = String.sub str index 2 in
      try
        let day = int_of_string day_str in
        day >= 1 && day <= 31
      with _ -> false
    else false
  in

  let parse_year str index =
    if index + 4 <= String.length str then
      let year_str = String.sub str index 4 in
      try
        let year = int_of_string year_str in
        year >= 1000 && year <= 9999
      with _ -> false
    else false
  in

  let parse_separator str index =
    if index < String.length str then
      match str.[index] with
      | '/' -> true
      | _ -> false
    else false
  in

  let parse_date str index =
    parse_month str index
    && parse_separator str (index + 2)
    && parse_day str (index + 3)
    && parse_separator str (index + 5)
    && parse_year str (index + 6)
  in

  parse_date s 0

(**code for the main menu*)
let rec main_menu user username password =
  ANSITerminal.erase Screen;

  let rec options_capabilities option =
    let rho option =
      ANSITerminal.erase Screen;

      let value = Options.rho option in
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ] "Below is your options Rhos value\n");
      Printf.printf "Your options Rho value is %s\n" (string_of_float value);
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ]
          "If you would like to return to the Main menu press 'M' if you would \
           like to return to the options main menu press any button\n");
      let menu = read_line_with_prompt "> " in
      match menu with
      | "M" -> main_menu user username password
      | _ -> options_capabilities option
    in
    let gamme option =
      ANSITerminal.erase Screen;

      let value = Options.gamma option in
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ] "Below is your options Gamma value\n");
      Printf.printf "Your options Gamma value is %s\n" (string_of_float value);
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ]
          "If you would like to return to the Main menu press 'M' if you would \
           like to return to the options main menu press any button\n");
      let menu = read_line_with_prompt "> " in
      match menu with
      | "M" -> main_menu user username password
      | _ -> options_capabilities option
    in

    let delta option =
      ANSITerminal.erase Screen;

      let value = Options.delta option in
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ] "Below is your options Delta value\n");
      Printf.printf "Your options Delta value is %s\n" (string_of_float value);
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ]
          "If you would like to return to the Main menu press 'M' if you would \
           like to return to the options main menu press any button\n");
      let menu = read_line_with_prompt "> " in
      match menu with
      | "M" -> main_menu user username password
      | _ -> options_capabilities option
    in
    let vega option =
      ANSITerminal.erase Screen;

      let value = Options.vega option in
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ] "Below is your options Vega value\n");
      Printf.printf "Your options Vega value is %s\n" (string_of_float value);
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ]
          "If you would like to return to the Main menu press 'M' if you would \
           like to return to the options main menu press any button\n");
      let menu = read_line_with_prompt "> " in
      match menu with
      | "M" -> main_menu user username password
      | _ -> options_capabilities option
    in
    let theta option =
      ANSITerminal.erase Screen;

      let value = Options.theta option in
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ] "Below is your options Theta value\n");
      Printf.printf "Your options Theta value is %s\n" (string_of_float value);
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ]
          "If you would like to return to the Main menu press 'M' if you would \
           like to return to the options main menu press any button\n");
      let menu = read_line_with_prompt "> " in
      match menu with
      | "M" -> main_menu user username password
      | _ -> options_capabilities option
    in
    let binomial_pricing option =
      ANSITerminal.erase Screen;

      let value = Options.binomial_price option in
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ]
          "We have ran the Binomial Price model on you option, below is the \
           result\n");
      Printf.printf "Your options price value is %s\n" (string_of_float value);
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ]
          "If you would like to return to the Main menu press 'M' if you would \
           like to return to the options main menu press any button\n");
      let menu = read_line_with_prompt "> " in
      match menu with
      | "M" -> main_menu user username password
      | _ -> options_capabilities option
    in
    let blac_schoels option =
      ANSITerminal.erase Screen;

      let value = Options.black_scholes_price option in
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ]
          "We have ran the Black Schoels model on you option, below is the \
           result\n");
      Printf.printf "Your options price value is %s\n" (string_of_float value);
      ANSITerminal.(
        printf [ Bold; Foreground Cyan ]
          "If you would like to return to the Main menu press 'M' if you would \
           like to return to the options main menu press any button\n");
      let menu = read_line_with_prompt "> " in
      match menu with
      | "M" -> main_menu user username password
      | _ -> options_capabilities option
    in

    ANSITerminal.erase Screen;
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Welcome to the PSI Capital options menu\n");
    Printf.printf "To run the Black Schoels model on your option press 'BS'\n";
    Printf.printf
      "To run the Binomial pricing model on your option press 'Bin'\n";
    Printf.printf "To get your options Rho value press 'R'\n";
    Printf.printf "To get your options Gamma value press 'G'\n";
    Printf.printf "To get your options Theta value press 'T'\n";
    Printf.printf "To get your options Delta value press 'D'\n";
    Printf.printf "To get your options Vega value press 'V'\n";
    let user_input = read_line_with_prompt "> " in
    match String.uppercase_ascii user_input with
    | "BS" -> blac_schoels option
    | "BIN" -> binomial_pricing option
    | "R" -> rho option
    | "G" -> gamme option
    | "T" -> theta option
    | "D" -> delta option
    | "V" -> vega option
    | _ -> main_menu user username password
  in
  let create_contract () =
    ANSITerminal.erase Screen;
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "To create an options contract please first enter the stock you would \
         like it to be based on\n");
    let underlying = read_line_with_prompt "> " in
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ] "Next enter your desired strike price\n");
    let strike = read_line_with_prompt "> " in
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Next enter your desired time to expiration in days price\n");
    let time = read_line_with_prompt "> " in
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ] "Finally enter if it is a call or put\n");
    let type_o = read_line_with_prompt "> " in
    match type_o with
    | "put" | "Put" -> (
        let option =
          Options.create_contract
            (StockScraping.get_price
               (StockScraping.get_ticker_info
                  (String.uppercase_ascii underlying)))
            (float_of_string strike) (float_of_string time) 0.5 0.5 Put 5
        in
        ANSITerminal.erase Screen;

        Options.print_option_contract option;
        ANSITerminal.(
          printf [ Bold; Foreground Cyan ]
            "Now that you have an option press any button to move on to the \
             option menu \n");
        let menu = read_line_with_prompt "> " in
        match menu with
        | _ -> options_capabilities option)
    | "Call" | "call" -> (
        let option =
          Options.create_contract
            (StockScraping.get_price
               (StockScraping.get_ticker_info
                  (String.uppercase_ascii underlying)))
            (float_of_string strike) (float_of_string time) 0.5 0.5 Call 5
        in
        ANSITerminal.erase Screen;

        Options.print_option_contract option;
        ANSITerminal.(
          printf [ Bold; Foreground Cyan ]
            "Now that you have an option press any x to move on to the option \
             menu \n");
        let menu = read_line_with_prompt "> " in
        match menu with
        | _ -> options_capabilities option)
    | _ -> (
        print_endline "invalid contract";
        ANSITerminal.(
          printf [ Bold; Foreground Cyan ]
            "Press any button to return to the main menu\n");
        let menu = read_line_with_prompt "> " in
        match menu with
        | _ -> main_menu user username password)
  in

  let options_functionality () =
    ANSITerminal.erase Screen;

    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Welcome to the PSI Capital options chain\n");
    Printf.printf "Lets start by creating an option! \n";
    Printf.printf "To create an option press 'O'\n";
    Printf.printf
      "If you would like to return to the main menu press any other button\n";

    Printf.printf "%s\n" (String.make 75 '-');
    let user_input = read_line_with_prompt "> " in
    match String.uppercase_ascii user_input with
    | "O" ->
        Printf.printf "You selected to look up the price of a stock.\n";
        create_contract ()
    | _ -> main_menu user username password
  in

  let get_info stock =
    ANSITerminal.erase Screen;

    ANSITerminal.(
      printf [ Bold; Foreground Cyan ] "Here is the info on %s stock !\n" stock);
    print_string StockScraping.(to_string (get_ticker_info stock));
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Press any button to return to the main menu \n");
    let user_input = read_line () in
    match String.uppercase_ascii user_input with
    | _ -> main_menu user username password
  in

  let purchase stock =
    ANSITerminal.erase Screen;
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Please enter the number of shares you would like to buy!\n");
    let user_input = read_line_with_prompt "> " in
    match String.uppercase_ascii user_input with
    | num -> (
        let created_stock =
          User.make_stock stock
            StockScraping.(get_price (get_ticker_info stock))
        in
        let new_port =
          User.buy created_stock (int_of_string num) (User.get_portfolio user)
        in
        ANSITerminal.erase Screen;
        ANSITerminal.(
          printf [ Bold; Foreground Cyan ]
            "Purchase confirmed, here is your current portfolio !\n");
        User.print_portfolio new_port;
        ANSITerminal.(
          printf [ Bold; Foreground Cyan ]
            "Press any button to return to the main menu \n");
        let user_input = read_line () in
        let new_user =
          User.make_user_from_portfolio username password new_port
        in
        match String.uppercase_ascii user_input with
        | _ -> main_menu new_user username password)
  in

  let purchase_confirmation stock =
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ] "Here is the info on %s stock !\n" stock);
    print_string StockScraping.(to_string (get_ticker_info stock));
    ANSITerminal.printf [ Bold; Foreground Cyan ]
      "Press C to confirm your purchase or any other button to return to the \
       main menu \n";
    let user_input = read_line () in
    match String.uppercase_ascii user_input with
    | "C" -> purchase stock
    | _ -> main_menu user username password
  in

  let lookup () =
    ANSITerminal.erase Screen;
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Please enter the ticker of the stock you would like to lookup!\n");
    let user_input = read_line_with_prompt "> " in
    match String.uppercase_ascii user_input with
    | x -> get_info x
  in
  let buy_screen () =
    ANSITerminal.erase Screen;
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Please enter the ticker of the stock you would like to buy");
    let user_input = read_line_with_prompt "> " in
    match String.uppercase_ascii user_input with
    | x -> purchase_confirmation x
  in

  let sell stock =
    ANSITerminal.erase Screen;
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Please enter the number of shares you would like to sell!\n");
    let user_input = read_line_with_prompt "> " in
    match String.uppercase_ascii user_input with
    | num -> (
        let new_port =
          User.sell stock (int_of_string num) (User.get_portfolio user)
        in
        ANSITerminal.erase Screen;
        ANSITerminal.(
          printf [ Bold; Foreground Cyan ]
            "Purchase confirmed, here is your current portfolio !\n");
        User.print_portfolio new_port;
        ANSITerminal.(
          printf [ Bold; Foreground Cyan ]
            "Press any button to return to the main menu \n");
        let user_input = read_line () in
        let new_user =
          User.make_user_from_portfolio username password new_port
        in
        match String.uppercase_ascii user_input with
        | _ -> main_menu new_user username password)
  in

  let sell_confirmation stock =
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ] "Here is the info on %s stock !\n" stock);
    print_string StockScraping.(to_string (get_ticker_info stock));
    ANSITerminal.printf [ Bold; Foreground Cyan ]
      "Press C to confirm your sale or any other button to return to the main \
       menu \n";
    let user_input = read_line () in
    match String.uppercase_ascii user_input with
    | "C" -> sell stock
    | _ -> main_menu user username password
  in

  let sell_screen () =
    ANSITerminal.erase Screen;
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Please enter the ticker of the stock you would like to sell");
    let user_input = read_line_with_prompt "> " in
    match String.uppercase_ascii user_input with
    | x -> sell_confirmation x
  in
  let show_portfolio () =
    ANSITerminal.erase Screen;
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ] "Here is your current portfolio !\n");
    User.print_portfolio (User.get_portfolio user);
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Press any button to return to the main menu \n");
    let user_input = read_line () in
    match String.uppercase_ascii user_input with
    | _ -> main_menu user username password
  in

  let backtest_run days =
    let port = User.get_portfolio user in
    let backtestable = User.to_backtest port in
    let change = Backtest.change_of_portfolio_adjust backtestable days in
    ANSITerminal.erase Screen;
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "With this portfolio, your profit/loss over %d days would be %.2f\n"
        days change);
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Press any button to return to the main menu \n");
    let user_input = read_line () in
    match String.uppercase_ascii user_input with
    | _ -> main_menu user username password
  in

  let backtest_screen () =
    ANSITerminal.erase Screen;
    ANSITerminal.(
      printf [ Bold; Foreground Cyan ]
        "Welcome to the Vestra Platform PSI capitals proprietary backtesting \
         platform to backtest your portfolio please enter an amount of days \
         between 2 and 1258 to backtest your stocks on ");
    let user_input = read_line_with_prompt "> " in
    match String.uppercase_ascii user_input with
    | x -> backtest_run (int_of_string x)
  in

  ANSITerminal.erase Screen;
  Printf.printf "%s\n" (String.make 75 '-');
  ANSITerminal.(
    printf [ Bold; Foreground Cyan ] "Welcome to PSI Capital, %s!\n" username);
  Printf.printf "Lets start by building you a portfolio!\n";
  Printf.printf "If you would like to lookup a stock press 'L'\n";
  Printf.printf "If you would like to buy a stock press 'B'\n";
  Printf.printf "If you would like to sell a stock press 'S'\n";
  Printf.printf "If you would like to view your portfolio press 'P'\n";
  Printf.printf "If you would like to backtest your portfolio enter 'Test'\n";
  Printf.printf "If you would like to lookup a stock option 'O'\n";
  Printf.printf "%s\n" (String.make 75 '-');

  let user_input = read_line_with_prompt "> " in
  match String.uppercase_ascii user_input with
  | "L" ->
      Printf.printf "You selected to look up the price of a stock.\n";
      lookup ()
  | "B" ->
      Printf.printf "You selected to buy a stock.\n";
      buy_screen ()
  | "S" ->
      Printf.printf "You selected to buy a stock.\n";
      sell_screen ()
  | "P" ->
      Printf.printf "You selected to buy a stock.\n";
      show_portfolio ()
  | "TEST" ->
      Printf.printf "You selected to backtest a stock.\n";
      backtest_screen ()
  | "O" ->
      Printf.printf "You selected to backtest a stock.\n";
      options_functionality ()
  (* Add stock lookup logic here *)
  | _ ->
      Printf.printf "Invalid option. Please try again.\n";
      main_menu user "hello" "woel"

(**function which presents the login screen allowing a user to login to their
   account*)
let login_screen () =
  ANSITerminal.erase Screen;

  Printf.printf "%s\n" (String.make 75 '-');
  ANSITerminal.(print_string [ Bold; Foreground Cyan ] "      Login Page\n");
  Printf.printf "%s\n\n" (String.make 75 '-');

  let username = read_line_with_prompt "Enter username: " in
  Printf.printf "Welcome, %s!\n\n" username;
  let password = read_line_with_prompt "Enter password: " in
  let user = User.make_user username password in
  Printf.printf "Login successful!\n";
  Printf.printf "%s\n" (String.make 75 '-');
  print_endline password;
  main_menu user username password

(**Function where the user decides if they are a returning user, or if they
   would like to create a new account*)
let rec login_or_new () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Welcome to PSI Capital! Please enter login to access your login information\n";
  let response = read_line () in
  match response with
  | "login" | "Login" -> login_screen ()
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Please enter 'login' to login ";
      login_or_new ()

(**Helper method to get the currrent year (used to compute if the user is over
   18 )*)
let get_current_year () =
  let open Unix in
  let curr = time () in
  let time = localtime curr in
  time.tm_year + 1900

(**Check age function that takes in a birthdate in the format mm/dd/yyyy and
   checks if the user is over the age of 18 *)
let check_age age =
  let len = String.length age in
  let year = int_of_string (String.sub age (len - 4) 4) in
  match year with
  | x when get_current_year () - x >= 18 ->
      ANSITerminal.print_string [ ANSITerminal.green ] "You are old enough \n";
      login_or_new ()
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Unfortunately, you are not old enough to use our platform\n"

(**Age function that gets the users age and send that information to the
   check_age function *)
let rec age () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Before you can start using our platform, we will need to verify some of \
     your information. Please enter your age in the format mm/dd/yyyy ";
  print_string "> ";
  let age_form = read_line () in
  let valid_age = is_valid_date_format age_form in
  match valid_age with
  | true -> check_age age_form
  | false -> (
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "This is invalid format please input the date in the format mm/dd/yyyy \
         press any button to try again";
      let response = read_line () in
      match response with
      | _ -> age ())

(**Terms and condititons methos that opens a file, reads it and outputs it to
   the user using a created file reader, the user has a choice to agree or
   disagree to the terms and conditons. If the user agrees they are sent to the
   age verification step, if not a message that the user may not use this
   platform without agreeing is presented*)
let rec terms_and_cond () =
  ANSITerminal.erase Screen;
  let terms_andcond = file_reader "Startup/terms.txt" in
  List.iter (fun line -> print_endline line) terms_andcond;

  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Enter Y if you agree, N if you do not";
  print_string "> ";
  match read_line () with
  | "Y" -> age ()
  | "N" ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Unfortunately, you must agree to the terms to use the platform"
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "This is not a valid respose\n";
      terms_and_cond ()

(**Main fucntion that starts up the program and sends a user to the terms and
   condtions*)
let psi_ascii_art =
  "  _____   _____ _    _____                _ _        _ \n"
  ^ " |  __ \\ / ____(_)  / ____|   /\\         (_) |      | |\n"
  ^ " | |__) | (___  _  | |       /  \\   _ __  _| |_ __ _| |\n"
  ^ " |  ___/ \\___ \\| | | |      / /\\ \\ | '_ \\| | __/ _` | |\n"
  ^ " | |     ____) | | | |____ / ____ \\| |_) | | || (_| | |\n"
  ^ " |_|    |_____/|_|  \\_____/_/    \\_\\ .__/|_|\\__\\__,_|_|\n"
  ^ "                                   | |                 \n"
  ^ "                                   |_|                 \n"

let main () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.blue ] psi_ascii_art;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to PSI Capital Management .\n";
  print_endline
    "Thank you for choosing PSI Capital. We're here to support your trading \
     journey.";
  print_endline "";

  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Before you can start using our platform, we will need to verify some of \
     your information. Please press enter to continue ";
  print_string "> ";
  match read_line () with
  | _ -> terms_and_cond ()

let () = main ()
