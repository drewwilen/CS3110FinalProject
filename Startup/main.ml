open StockScraping

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

(**function to make lines bold in the terminal*)
let read_line_with_prompt prompt =
  ANSITerminal.(print_string [ Bold ] prompt);
  flush stdout;
  input_line stdin

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
  Printf.printf "Login successful!\n";
  Printf.printf "%s\n" (String.make 75 '-');
  print_endline password

(**Function where the user decides if they are a returning user, or if they
   would like to create a new account*)
let rec login_or_new () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Welcome to PSI Capital! If you are a returning user type login, if you \
     would like to create an account type create ";
  let response = read_line () in
  match response with
  | "login" | "Login" -> login_screen ()
  | "create" | "Create" -> failwith "create"
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Please enter a valid command";
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
let age () =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Before you can start using our platform, we will need to verify some of \
     your information. Please enter your age in the format mm/dd/yyyy ";
  print_string "> ";
  let age = read_line () in
  let valid_age = is_valid_date_format age in
  match valid_age with
  | true -> check_age age
  | false ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "This is invalid format please input the date in the format mm/dd/yyyy "

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
  | _ -> (
      terms_and_cond ();

      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Enter a stock ticker to check ";
      match read_line () with
      | e -> print_string StockScraping.(to_string (get_ticker_info e)))

let () = main ()
