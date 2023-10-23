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
      ANSITerminal.print_string [ ANSITerminal.red ]
        "You are old enough, (next step not implemented)"
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Unfortunately, you are not old enough to use our platform\n"

(**Age function that gets the users age and send that information to the
   check_age function *)
let age () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Before you can start using our platform, we will need to verify some of \
     your information. Please enter your age in the format mm/dd/yyyy ";
  print_string "> ";
  match read_line () with
  | x -> check_age x

(**Terms and condititons methos that opens a file, reads it and outputs it to
   the user using a created file reader, the user has a choice to agree or
   disagree to the terms and conditons. If the user agrees they are sent to the
   age verification step, if not a message that the user may not use this
   platform without agreeing is presented*)
let rec terms_and_cond () =
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
      print_string "This is not a valid respose";
      terms_and_cond ()

(**Main fucntion that starts up the program and sends a user to the terms and
   condtions*)
let main () =
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
