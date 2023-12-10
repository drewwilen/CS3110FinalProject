let read_csv_file (file_path : string) : string list list =
  let channel = open_in file_path in
  let csv_channel = Csv.of_channel ~separator:',' channel in
  let rows = Csv.input_all csv_channel in
  rows

let s_and_p =
  [
    "MMM";
    "ABT";
    "ABBV";
    "ACN";
    "ATVI";
    "AYI";
    "ADBE";
    "AMD";
    "AAP";
    "AES";
    "AET";
    "AMG";
    "AFL";
    "A";
    "APD";
    "AKAM";
    "ALK";
    "ALB";
    "ARE";
    "ALXN";
    "ALGN";
    "ALLE";
    "AGN";
    "ADS";
    "LNT";
    "ALL";
    "GOOGL";
    "GOOG";
    "MO";
    "AMZN";
    "AEE";
    "AAL";
    "AEP";
    "AXP";
    "AIG";
    "AMT";
    "AWK";
    "AMP";
    "ABC";
    "AME";
    "AMGN";
    "APH";
    "APC";
    "ADI";
    "ANDV";
    "ANSS";
    "ANTM";
    "AON";
    "AOS";
    "APA";
    "AIV";
    "AAPL";
    "AMAT";
    "APTV";
    "ADM";
    "ARNC";
    "AJG";
    "AIZ";
    "T";
    "ADSK";
    "ADP";
    "AZO";
    "AVB";
    "AVY";
    "BHGE";
    "BLL";
    "BAC";
    "BK";
    "BAX";
    "BBT";
    "BDX";
    "BRK.B";
    "BBY";
    "BIIB";
    "BLK";
    "HRB";
    "BA";
    "BWA";
    "BXP";
    "BSX";
    "BHF";
    "BMY";
    "AVGO";
    "BF.B";
    "CHRW";
    "CA";
    "COG";
    "CDNS";
    "CPB";
    "COF";
    "CAH";
    "CBOE";
    "KMX";
    "CCL";
    "CAT";
    "CBG";
    "CBS";
    "CELG";
    "CNC";
    "CNP";
    "CTL";
    "CERN";
    "CF";
    "SCHW";
    "CHTR";
    "CHK";
    "CVX";
    "CMG";
    "CB";
    "CHD";
    "CI";
    "XEC";
    "CINF";
    "CTAS";
    "CSCO";
    "C";
    "CFG";
    "CTXS";
    "CLX";
    "CME";
    "CMS";
    "KO";
    "CTSH";
    "CL";
    "CMCSA";
    "CMA";
    "CAG";
    "CXO";
    "COP";
    "ED";
    "STZ";
    "COO";
    "GLW";
    "COST";
    "COTY";
    "CCI";
    "CSRA";
    "CSX";
    "CMI";
    "CVS";
    "DHI";
    "DHR";
    "DRI";
    "DVA";
    "DE";
    "DAL";
    "XRAY";
    "DVN";
    "DLR";
    "DFS";
    "DISCA";
    "DISCK";
    "DISH";
    "DG";
    "DLTR";
    "D";
    "DOV";
    "DWDP";
    "DPS";
    "DTE";
    "DRE";
    "DUK";
    "DXC";
    "ETFC";
    "EMN";
    "ETN";
    "EBAY";
    "ECL";
    "EIX";
    "EW";
    "EA";
    "EMR";
    "ETR";
    "EVHC";
    "EOG";
    "EQT";
    "EFX";
    "EQIX";
    "EQR";
    "ESS";
    "EL";
    "ES";
    "RE";
    "EXC";
    "EXPE";
    "EXPD";
    "ESRX";
    "EXR";
    "XOM";
    "FFIV";
    "FB";
    "FAST";
    "FRT";
    "FDX";
    "FIS";
    "FITB";
    "FE";
    "FISV";
    "FLIR";
    "FLS";
    "FLR";
    "FMC";
    "FL";
    "F";
    "FTV";
    "FBHS";
    "BEN";
    "FCX";
    "GPS";
    "GRMN";
    "IT";
    "GD";
    "GE";
    "GGP";
    "GIS";
    "GM";
    "GPC";
    "GILD";
    "GPN";
    "GS";
    "GT";
    "GWW";
    "HAL";
    "HBI";
    "HOG";
    "HRS";
    "HIG";
    "HAS";
    "HCA";
    "HCP";
    "HP";
    "HSIC";
    "HSY";
    "HES";
    "HPE";
    "HLT";
    "HOLX";
    "HD";
    "HON";
    "HRL";
    "HST";
    "HPQ";
    "HUM";
    "HBAN";
    "HII";
    "IDXX";
    "INFO";
    "ITW";
    "ILMN";
    "IR";
    "INTC";
    "ICE";
    "IBM";
    "INCY";
    "IP";
    "IPG";
    "IFF";
    "INTU";
    "ISRG";
    "IVZ";
    "IQV";
    "IRM";
    "JEC";
    "JBHT";
    "SJM";
    "JNJ";
    "JCI";
    "JPM";
    "JNPR";
    "KSU";
    "K";
    "KEY";
    "KMB";
    "KIM";
    "KMI";
    "KLAC";
    "KSS";
    "KHC";
    "KR";
    "LB";
    "LLL";
    "LH";
    "LRCX";
    "LEG";
    "LEN";
    "LUK";
    "LLY";
    "LNC";
    "LKQ";
    "LMT";
    "L";
    "LOW";
    "LYB";
    "MTB";
    "MAC";
    "M";
    "MRO";
    "MPC";
    "MAR";
    "MMC";
    "MLM";
    "MAS";
    "MA";
    "MAT";
    "MKC";
    "MCD";
    "MCK";
    "MDT";
    "MRK";
    "MET";
    "MTD";
    "MGM";
    "KORS";
    "MCHP";
    "MU";
    "MSFT";
    "MAA";
    "MHK";
    "TAP";
    "MDLZ";
    "MON";
    "MNST";
    "MCO";
    "MS";
    "MOS";
    "MSI";
    "MYL";
    "NDAQ";
    "NOV";
    "NAVI";
    "NTAP";
    "NFLX";
    "NWL";
    "NFX";
    "NEM";
    "NWSA";
    "NWS";
    "NEE";
    "NLSN";
    "NKE";
    "NI";
    "NBL";
    "JWN";
    "NSC";
    "NTRS";
    "NOC";
    "NCLH";
    "NRG";
    "NUE";
    "NVDA";
    "ORLY";
    "OXY";
    "OMC";
    "OKE";
    "ORCL";
    "PCAR";
    "PKG";
    "PH";
    "PDCO";
    "PAYX";
    "PYPL";
    "PNR";
    "PBCT";
    "PEP";
    "PKI";
    "PRGO";
    "PFE";
    "PCG";
    "PM";
    "PSX";
    "PNW";
    "PXD";
    "PNC";
    "RL";
    "PPG";
    "PPL";
    "PX";
    "PCLN";
    "PFG";
    "PG";
    "PGR";
    "PLD";
    "PRU";
    "PEG";
    "PSA";
    "PHM";
    "PVH";
    "QRVO";
    "PWR";
    "QCOM";
    "DGX";
    "RRC";
    "RJF";
    "RTN";
    "O";
    "RHT";
    "REG";
    "REGN";
    "RF";
    "RSG";
    "RMD";
    "RHI";
    "ROK";
    "COL";
    "ROP";
    "ROST";
    "RCL";
    "CRM";
    "SBAC";
    "SCG";
    "SLB";
    "SNI";
    "STX";
    "SEE";
    "SRE";
    "SHW";
    "SIG";
    "SPG";
    "SWKS";
    "SLG";
    "SNA";
    "SO";
    "LUV";
    "SPGI";
    "SWK";
    "SBUX";
    "STT";
    "SRCL";
    "SYK";
    "STI";
    "SYMC";
    "SYF";
    "SNPS";
    "SYY";
    "TROW";
    "TPR";
    "TGT";
    "TEL";
    "FTI";
    "TXN";
    "TXT";
    "TMO";
    "TIF";
    "TWX";
    "TJX";
    "TMK";
    "TSS";
    "TSCO";
    "TDG";
    "TRV";
    "TRIP";
    "FOXA";
    "FOX";
    "TSN";
    "UDR";
    "ULTA";
    "USB";
    "UAA";
    "UA";
    "UNP";
    "UAL";
    "UNH";
    "UPS";
    "URI";
    "UTX";
    "UHS";
    "UNM";
    "VFC";
    "VLO";
    "VAR";
    "VTR";
    "VRSN";
    "VRSK";
    "VZ";
    "VRTX";
    "VIAB";
    "V";
    "VNO";
    "VMC";
    "WMT";
    "WBA";
    "DIS";
    "WM";
    "WAT";
    "WEC";
    "WFC";
    "HCN";
    "WDC";
    "WU";
    "WRK";
    "WY";
    "WHR";
    "WMB";
    "WLTW";
    "WYN";
    "WYNN";
    "XEL";
    "XRX";
    "XLNX";
    "XL";
    "XYL";
    "YUM";
    "ZBH";
    "ZION";
    "ZTS";
  ]

(* file path to all stock data csv *)
(* let all_stocks_path = "data/all_stock_5yr.csv" *)
(* file path to each individual stock's data *)
let create_file_path_individual ticker : string =
  "data/individual_stocks_5yr/individual_stocks_5yr/" ^ ticker ^ "_data.csv"

(* Path names:
   "data/individual_stocks_5yr/individual_stocks_5yr/TICKER_data.csv"
   ""data/all_stock_5yr.csv"" We remove the first element as that row contains
   no data *)
let create_list ticker =
  read_csv_file (create_file_path_individual ticker) |> List.tl

(* Print the data for the stock in the order of:
   Name,date,open,high,low,close,volume, *)
let rec print_data lst =
  match lst with
  | [] -> print_endline ""
  | h :: t -> (
      match h with
      | [ date; ope_n; high; low; close; volume; name ] ->
          print_endline ("Ticker: " ^ name);
          print_endline ("Date: " ^ date);
          print_endline ("Open: " ^ ope_n);
          print_endline ("high: " ^ high);
          print_endline ("low: " ^ low);
          print_endline ("close: " ^ close);
          print_endline ("volume: " ^ volume);
          print_data t
      | _ -> failwith "invalid\n\n form")

let valid_stocks (stocks : string list) : string list =
  List.filter (fun x -> List.mem x s_and_p) stocks

let rec first_n_elements lst n =
  match (lst, n) with
  | [], _ -> []
  | _, 0 -> []
  | h :: t, n -> h :: first_n_elements t (n - 1)

let historical_stock_data ticker days interval =
  let safe_ticker =
    match valid_stocks [ ticker ] with
    | [ stock ] -> stock
    | _ -> failwith "invalid stock"
  in
  let data_list = create_list safe_ticker in
  let first_elements = first_n_elements data_list days in
  List.filteri (fun i _ -> (i + 1) mod interval = 0) first_elements

let rec historial_n_stocks_data tickers days interval =
  match tickers with
  | [] -> []
  | h :: t ->
      historical_stock_data h days interval
      :: historial_n_stocks_data t days interval

let change_of_stock ticker days =
  match historical_stock_data ticker days (days / 2) with
  | [] -> 0.0
  | h :: t ->
      let close1 =
        match h with
        | [ _; _; _; _; close; _; _ ] -> close
        | _ -> failwith "wrong format"
      in
      let close2 =
        match t with
        | [ _; _; _; _; close; _; _ ] :: [] -> close
        | _ -> failwith "wrong format"
      in
      float_of_string close2 -. float_of_string close1

let rec change_of_portfolio portfolio days =
  match portfolio with
  | [] -> 0.0
  | h :: t -> change_of_stock h days +. change_of_portfolio t days

let rec change_of_portfolio_adjust (portfolio_shares : (string * int) list) days
    =
  match portfolio_shares with
  | [] -> 0.0
  | (tick, shares) :: t ->
      (float_of_int shares *. change_of_stock tick days)
      +. change_of_portfolio_adjust t days
