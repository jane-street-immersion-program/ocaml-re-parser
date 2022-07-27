open Core
open Parser
open Parser.Let_syntax

let year = times (Char.digit |> ignore_m) 4
let slash = Char.one_of [ '/' ] |> ignore_m
let dash = Char.one_of [ '-' ] |> ignore_m
let two_digits = times (Char.digit |> ignore_m) 2
let three_letter_month = times (Char.alpha |> ignore_m) 3
let space = Char.space |> ignore_m
let colon = Char.one_of [ ':' ] |> ignore_m
let capital_t = Char.one_of [ 'T' ] |> ignore_m
let dot = Char.one_of [ '.' ] |> ignore_m
let one_or_more_digits = Char.digit |> ignore_m |> repeat ~min:1
let z = Char.one_of [ 'Z' ] |> ignore_m
let plus_or_minus = Char.one_of [ '+'; '-' ] |> ignore_m

let tzd =
  capture
    (let%map () = plus_or_minus
     and () = two_digits
     and () = colon
     and () = two_digits in
     ())
;;

module Date_parser = struct
  let y_slash_m_slash_d =
    capture
      (let%map () = year
       and () = slash
       and () = two_digits
       and () = slash
       and () = two_digits in
       ())
  ;;

  let m_slash_d_slash_y =
    capture
      (let%map () = two_digits
       and () = slash
       and () = two_digits
       and () = slash
       and () = year in
       ())
  ;;

  let y_dash_m_dash_d =
    capture
      (let%map () = year
       and () = dash
       and () = two_digits
       and () = dash
       and () = two_digits in
       ())
  ;;

  let y_m_d_no_space =
    capture
      (let%map () = year
       and () = two_digits
       and () = two_digits in
       ())
  ;;

  let d_month_y_space_inbetween =
    capture
      (let%map () = two_digits
       and () = space
       and () = three_letter_month
       and () = space
       and () = year in
       ())
  ;;

  let y_month_day_space_inbetween =
    capture
      (let%map () = year
       and () = space
       and () = three_letter_month
       and () = space
       and () = two_digits in
       ())
  ;;

  let d_month_y_no_space =
    capture
      (let%map () = two_digits
       and () = three_letter_month
       and () = year in
       ())
  ;;

  let date_parser =
    let%map () = start_of_input
    and () = any_string |> ignore_m
    and current_date =
      Parser.or_
        [ y_slash_m_slash_d
        ; m_slash_d_slash_y
        ; y_dash_m_dash_d
        ; y_m_d_no_space
        ; d_month_y_space_inbetween
        ; y_month_day_space_inbetween
        ; d_month_y_no_space
        ]
    and () = any_string |> ignore_m
    and () = end_of_input in
    Date.of_string current_date
  ;;
end

let date_parser = Date_parser.date_parser

let time_parser =
  let utc =
    capture
      (let%map () = Date_parser.y_dash_m_dash_d |> ignore_m
       and () = or_ [ space; capital_t ]
       and () = two_digits
       and () = colon
       and () = two_digits
       and () = colon
       and () = two_digits
       and () = dot
       and () = one_or_more_digits
       and () = or_ [ z; tzd |> ignore_m ] in
       ())
  in
  let%map () = start_of_input
  and () = any_string |> ignore_m |> repeat
  and time = utc
  and () = any_string |> ignore_m
  and () = end_of_input in
  Time_ns_unix.of_string_with_utc_offset time
;;
