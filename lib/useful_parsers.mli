open Core

val year : unit Parser.t
val slash : unit Parser.t
val dash : unit Parser.t
val two_digits : unit Parser.t
val three_letter_month : unit Parser.t
val space : unit Parser.t
val colon : unit Parser.t
val t : unit Parser.t
val dot : unit Parser.t
val one_or_more_digits : unit Parser.t
val z : unit Parser.t
val plus_or_minus : unit Parser.t
val tzd : string Parser.t
val date_parser : Date.t Parser.t
val time_parser: Time_ns_unix.t Parser.t




