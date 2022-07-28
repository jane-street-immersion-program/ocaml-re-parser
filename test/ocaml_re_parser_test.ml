open! Core

module Test_equality
    (P1 : Ocaml_re_parser.Re2_compatible_parser_intf.S)
    (P2 : Ocaml_re_parser.Re2_compatible_parser_intf.S) =
struct
  let%expect_test "Char" =
    let parsers =
      let module C1 = P1.Char in
      let module C2 = P2.Char in
      [ C1.any, C2.any, "any"
      ; C1.digit, C2.digit, "digit"
      ; C1.space, C2.space, "space"
      ; C1.upper, C2.upper, "upper"
      ; C1.lower, C2.lower, "lower"
      ; C1.alpha, C2.alpha, "alpha"
      ; C1.alnum, C2.alnum, "alnum"
      ; C1.one_of [ 'a' ], C2.one_of [ 'a' ], "one_of [a]"
      ; C1.one_of [ 'a'; '-'; 'z' ], C2.one_of [ 'a'; '-'; 'z' ], "one_of [a\\-z]"
      ; C1.not_one_of [ 'a' ], C2.not_one_of [ 'a' ], "not_one_of [a]"
      ; ( C1.not_one_of [ 'a'; '-'; 'z' ]
        , C2.not_one_of [ 'a'; '-'; 'z' ]
        , "not_one_of [a\\-z]" )
      ]
    in
    let strings =
      [ "a"
      ; "b"
      ; "c"
      ; "more than 1 chr"
      ; ""
      ; "1"
      ; "24"
      ; "3"
      ; "d"
      ; "UPPER"
      ; "lower"
      ; "UpPeR"
      ; "lOwEr"
      ; "sjg24eA2"
      ]
      @ List.map Char.all ~f:Char.to_string
    in
    List.iter parsers ~f:(fun (p1, p2, parser_type) ->
        List.iter strings ~f:(fun chr ->
            let result1 = P1.run p1 chr in
            let result2 = P2.run p2 chr in
            match [%equal: char option] result1 result2 with
            | true -> ()
            | false ->
              print_s
                [%message
                  (parser_type : string)
                    (chr : string)
                    (result1 : char option)
                    (result2 : char option)]));
    [%expect {| |}]
  ;;
end

include Test_equality (Re2.Parser) (Ocaml_re_parser.Parser)

module Test (Parser : Ocaml_re_parser.Re2_compatible_parser_intf.S) = struct
  module Parser = struct
    include Parser

    (* Don't ask why I can't explain *)
    module Open_on_rhs_intf = struct
      module type S = Ocaml_re_parser.Re2_compatible_parser_intf.S with type 'a t = 'a t
    end

    include Applicative.Make_let_syntax (Parser) (Open_on_rhs_intf) (Parser)
  end

  let%expect_test "Char + Applicative" =
    let parser =
      let open Parser in
      let open Parser.Let_syntax in
      let%map digit1 = Char.digit
      and digit2 = Char.digit
      and _dot = Char.one_of [ '.' ]
      and digit3 = Char.digit in
      String.of_char_list [ digit1; digit2; digit3 ]
    in
    let string1 = "foo bar" in
    let string2 = "12.3" in
    let res1 = Parser.run parser string1 in
    let res2 = Parser.run parser string2 in
    print_s [%message (res1 : string option) (res2 : string option)];
    [%expect {| ((res1 ()) (res2 (123))) |}]
  ;;

  let%expect_test "Strings and stuff" =
    let parser =
      let open Parser in
      let open Parser.Let_syntax in
      let%map str = any_string in
      str
    in
    let strings =
      [ "the string"
      ; {|this should be able
      to match
      everything
      even over newlines|}
      ; {||}
      ]
    in
    List.iter strings ~f:(fun str ->
        let res = Parser.run parser str in
        print_s [%message (res : string option)]);
    [%expect
      {|
      (res ("the string"))
      (res
       ( "this should be able\
        \n      to match\
        \n      everything\
        \n      even over newlines"))
      (res ("")) |}]
  ;;

  let%expect_test "email extraction" =
    let parser =
      let open Parser in
      let open Parser.Let_syntax in
      let alnum_many = Char.alnum |> ignore_m |> repeat ~min:1 |> capture in
      let anything_and_whitespaces =
        let%map () = any_string |> ignore_m
        and () = Char.space |> ignore_m in
        ()
      in
      let%map () = start_of_input
      and () = repeat anything_and_whitespaces
      and email =
        let email_parser =
          let%map _email_address = alnum_many
          and () = Char.one_of [ '@' ] |> ignore_m
          and _domain = alnum_many
          and () = Char.one_of [ '.' ] |> ignore_m
          and _tld = alnum_many in
          ()
        in
        capture email_parser
      and () = any_string |> ignore_m
      and () = end_of_input in
      email
    in
    let strings =
      [ "lom4@cornell.edu"
      ; "my email is havin@gmail.com so you should go email me"
      ; "not an mail@email."
      ]
    in
    List.iter strings ~f:(fun str ->
        let res = Parser.run parser str in
        print_s [%message (res : string option)]);
    [%expect {|
    (res (lom4@cornell.edu))
    (res (havin@gmail.com))
    (res ()) |}]
  ;;

  let%expect_test "name" =
    let parser =
      let open Parser in
      let open Parser.Let_syntax in
      let alnum_many = Char.alnum |> ignore_m |> repeat ~min:1 |> capture in
      let name =
        capture
          (let%map () = Char.upper |> ignore_m
           and () = alnum_many |> ignore_m in
           ())
      in
      let%map () = start_of_input
      and () = any_string |> ignore_m |> repeat
      and full_name =
        let%map () = Char.one_of [ ':' ] |> ignore_m
        and () = Char.space |> ignore_m |> repeat
        and first_name = name
        and () = Char.space |> ignore_m |> repeat ~min:1
        and () = any_string |> ignore_m |> repeat
        and last_name = name in
        first_name ^ " " ^ last_name
      and () = any_string |> ignore_m
      and () = end_of_input in
      full_name
    in
    let strings =
      [ "Lukman Moyosore"
      ; "my name is  : Lukman . Moyosore and i blah blah blah"
      ; "my name is:Lukman   Moysore and i like trees"
      ]
    in
    List.iter strings ~f:(fun str ->
        let res = Parser.run parser str in
        print_s [%message (res : string option)]);
    [%expect
      {|
    (res ())
    (res ("Lukman Moyosore"))
    (res ("Lukman Moysore")) |}]
  ;;

  let%expect_test "matches" =
    let open Parser in
    let chars = [ "nocaps"; "   aaaa11 ALLCAPS"; ""; "\\^" ] in
    List.iter chars ~f:(fun chr ->
        let res = Parser.matches Char.upper chr in
        print_s [%message (res : bool)]);
    [%expect {|
    (res false)
    (res true)
    (res false)
    (res false) |}]
  ;;

  let%expect_test "fail" =
    let open Parser in
    let strings = [ "this should fail on any string"; "FAIL"; "" ] in
    List.iter strings ~f:(fun str ->
        let res = Parser.run fail str in
        match res with
        | Some _ -> failwith "this shouldnt happen"
        | None -> ())
  ;;

  let%expect_test "times" =
    let open Parser in
    let a_times_3 = times (Char.one_of [ 'a' ] |> ignore_m) 3 |> capture in
    let strings = [ "aaa"; "abaaaba"; "aa  b aaba" ] in
    List.iter strings ~f:(fun str ->
        let res = Parser.run a_times_3 str in
        print_s [%message (res : string option)]);
    [%expect {|
    (res (aaa))
    (res (aaa))
    (res ()) |}]
  ;;

  let%expect_test "or_" =
    let open Parser in
    let upper_or_digit =
      or_ [ Char.upper; Char.digit; Char.space ] |> ignore_m |> capture
    in
    let strings = [ "ddddD2"; "ddddd2 "; "dddd 2D"; "A 44"; ""; "d,dlkg==" ] in
    List.iter strings ~f:(fun str ->
        let res = Parser.run upper_or_digit str in
        print_s [%message (res : string option)]);
    [%expect
      {|
    (res (D))
    (res (2))
    (res (" "))
    (res (A))
    (res ())
    (res ()) |}]
  ;;

  let%expect_test "Decimal Module" =
    let open Parser in
    let ints = Decimal.int in
    let strings = [ ""; "-10"; "+005"; "42" ] in
    List.iter strings ~f:(fun str ->
        let res = Parser.run ints str in
        print_s [%message (res : int option)]);
    [%expect {|
  (res ())
  (res (-10))
  (res (5))
  (res (42)) |}]
  ;;
end

include Test (Re2.Parser)
include Test (Ocaml_re_parser.Parser)
