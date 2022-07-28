open! Core
open Ocaml_re_parser

let%expect_test "dates" =
  let strings =
    [ "todays date is: 2022/07/22 and it is very hot today"
    ; "todays date is: 07/22/2022 and it is very hot today"
    ; "todays date is: 2022-07-22 and it is very hot today"
    ; "todays date is: 20220722 and it is very hot today"
    ; "todays date is: 22 Jul 2022 and it is very hot today"
    ; "todays date is: 2022 Jul 22 and it is very hot today"
    ; "todays date is: 22JUL2022 and it is very hot today"
    ; "todays date is: July 22nd 2022 and it is very hot today"
    ]
  in
  List.iter strings ~f:(fun str ->
      let res = Parser.run Useful_parsers.date_parser str in
      print_s [%message (res : Date.t option)]);
  [%expect
    {|
  (res (2022-07-22))
  (res (2022-07-22))
  (res (2022-07-22))
  (res (2022-07-22))
  (res (2022-07-22))
  (res (2022-07-22))
  (res (2022-07-22))
  (res ()) |}]
;;

let%expect_test "times" =
  let strings =
    [ "1997-07-16T19:20:30.45+01:00"
    ; "2022-07-26 17:38:26.698101760Z"
    ; "2022-07-26T17:38:26.698101760Z"
    ]
  in
  List.iter strings ~f:(fun str ->
      let res = Parser.run Useful_parsers.time_parser str in
      print_s [%message (res : Time_ns_unix.t option)]);
  [%expect
    {|
  (res ((1997-07-16 18:20:30.450000000Z)))
  (res ((2022-07-26 17:38:26.698101760Z)))
  (res ((2022-07-26 17:38:26.698101760Z))) |}]
;;
