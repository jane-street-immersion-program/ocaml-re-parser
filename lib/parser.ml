open! Core

module Body : sig
  include Parser_intf.S
end = struct
  module T = struct
    type 'a t =
      { regex : Re.t
      ; num_captures : int
      ; extract_value_from_captured_groups : Re.Group.t -> int -> 'a option
      }

    let return x =
      { regex = Re.epsilon
      ; num_captures = 0
      ; extract_value_from_captured_groups = (fun _ _ -> Some x)
      }
    ;;

    let map =
      `Custom
        (fun t ~f ->
          { t with
            extract_value_from_captured_groups =
              (fun x y -> Option.map (t.extract_value_from_captured_groups x y) ~f)
          })
    ;;

    let apply tf tx =
      let extract_value_from_captured_groups matches shift =
        let f = tf.extract_value_from_captured_groups matches shift in
        let x = tx.extract_value_from_captured_groups matches (shift + tf.num_captures) in
        Option.map2 f x ~f:(fun f x -> f x)
      in
      { regex = Re.seq [ tf.regex; tx.regex ]
      ; num_captures = tf.num_captures + tx.num_captures
      ; extract_value_from_captured_groups
      }
    ;;
  end

  include T
  include Applicative.Make (T)

  let get_first_string_of_group group offset = Re.Group.get_opt group offset

  module Char = struct
    module Re2_compatiblity = struct
      let upper =
        List.init 26 ~f:(fun i -> i + 65 |> Char.of_int_exn) |> String.of_char_list
      ;;

      let lower =
        List.init 26 ~f:(fun i -> i + 97 |> Char.of_int_exn) |> String.of_char_list
      ;;

      let digit =
        List.init 10 ~f:(fun i -> i + 48 |> Char.of_int_exn) |> String.of_char_list
      ;;

      let alpha = upper ^ lower
      let alnum = alpha ^ digit
    end

    let get_first_char_of_group group offset =
      Re.Group.get_opt group offset |> Option.map ~f:Char.of_string
    ;;

    let any : 'a t =
      { regex = Re.group Re.any
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let digit : 'a t =
      { regex = Re.group Re.digit
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let space : 'a t =
      { regex = Re.group Re.space
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let upper : 'a t =
      { regex = Re.group (Re.set Re2_compatiblity.upper)
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let lower : 'a t =
      { regex = Re.group (Re.set Re2_compatiblity.lower)
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let alpha : char t =
      { regex = Re.group (Re.set Re2_compatiblity.alpha)
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let alnum : char t =
      { regex = Re.group (Re.set Re2_compatiblity.alnum)
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let one_of : char list -> char t =
     fun chars ->
      { regex = Re.group (Re.set (String.of_char_list chars))
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
   ;;

    let not_one_of : char list -> char t =
     fun chars ->
      { regex = Re.group (Re.compl [ Re.set (String.of_char_list chars) ])
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
   ;;
  end

  let string (s : string) : unit t =
    { regex = Re.str s
    ; num_captures = 0
    ; extract_value_from_captured_groups = (fun _ _ -> Some ())
    }
  ;;

  let any_string : string t =
    { regex = Re.group (Re.rep Re.any)
    ; num_captures = 1
    ; extract_value_from_captured_groups = get_first_string_of_group
    }
  ;;

  let start_of_input : unit t =
    { regex = Re.bos
    ; num_captures = 0
    ; extract_value_from_captured_groups = (fun _ _ -> Some ())
    }
  ;;

  let end_of_input : unit t =
    { regex = Re.eos
    ; num_captures = 0
    ; extract_value_from_captured_groups = (fun _ _ -> Some ())
    }
  ;;

  let ignore_m : _ t -> unit t =
   fun t ->
    { regex = Re.no_group t.regex
    ; num_captures = t.num_captures
    ; extract_value_from_captured_groups = (fun _ _ -> Some ())
    }
 ;;

  let and_capture (t : 'a t) : ('a * string) t =
    { regex = Re.group t.regex
    ; num_captures = t.num_captures + 1
    ; extract_value_from_captured_groups =
        (fun group offset ->
          let full_capture : string option = get_first_string_of_group group offset in
          let previous_capture : 'a option =
            t.extract_value_from_captured_groups group (offset + 1)
          in
          Option.both previous_capture full_capture)
    }
  ;;

  let capture (t : unit t) : string t = map (and_capture t) ~f:Tuple2.get2

  let repeat ?(greedy = true) ?(min = 0) ?(max = None) (t : unit t) =
    let repeating_re = Re.repn t.regex min max in
    let re =
      match greedy with
      | true -> Re.greedy repeating_re
      | false -> Re.non_greedy repeating_re
    in
    { regex = re
    ; num_captures = t.num_captures
    ; extract_value_from_captured_groups = (fun _ _ -> Some ())
    }
  ;;

  let to_re ?(case_sensitive = true) (t : 'a t) =
    match case_sensitive with
    | true -> Re.case t.regex
    | false -> Re.no_case t.regex
  ;;

  let of_re t =
    { regex = Re.no_group t
    ; num_captures = 0
    ; extract_value_from_captured_groups = (fun _ _ -> Some ())
    }
  ;;

  let or_ (parsers : 'a t list) : 'a t =
    let regex_list =
      List.map parsers ~f:(fun t -> Re.seq [ Re.group Re.epsilon; t.regex ])
    in
    let new_regex = Re.alt regex_list in
    let extract_value_from_captured_groups group offset =
      let rec find_the_match (group : Re.Group.t) (offset : int) (parsers : 'a t list) =
        match parsers with
        | [] ->
          failwith
            "Re.Parser.or_.extract_value_from_captured_groups bug: called on non-match"
        | t :: ts ->
          if Option.is_some (Re.Group.get_opt group offset)
          then t.extract_value_from_captured_groups group (offset + 1)
          else find_the_match group (offset + 1 + t.num_captures) ts
      in
      find_the_match group offset parsers
    in
    { regex = new_regex
    ; num_captures = List.sum (module Int) parsers ~f:(fun t -> t.num_captures + 1)
    ; extract_value_from_captured_groups
    }
  ;;

  let times (t : unit t) (amt : int) = repeat ~min:amt ~max:(Some amt) t

  let compile_regex ?(case_sensitive = true) (t : 'a t) =
    let regex = to_re ~case_sensitive t in
    Re.compile regex
  ;;

  let compile ?(case_sensitive = true) (t : 'a t) =
    let rex = compile_regex ~case_sensitive t in
    Staged.stage (fun s ->
        Re.exec_opt rex s
        |> Option.bind ~f:(fun group -> t.extract_value_from_captured_groups group 1))
  ;;

  let run ?case_sensitive (t : 'a t) = Staged.unstage (compile ?case_sensitive t)

  let matches ?(case_sensitive = true) t =
    let rex = compile_regex ~case_sensitive t in
    fun input -> Re.execp rex input
  ;;

  let fail =
    { regex = Re.empty
    ; num_captures = 0
    ; extract_value_from_captured_groups =
        (fun _ _ -> failwith "BUG: This regex should match nothing.")
    }
  ;;

  let optional ?(greedy = true) (t : 'a t) : 'a option t =
    let greedy_or_not =
      match greedy with
      | true -> Re.greedy
      | false -> Re.non_greedy
    in
    let regex = t.regex |> Re.group |> Re.opt |> greedy_or_not in
    { regex
    ; num_captures = t.num_captures + 1
    ; extract_value_from_captured_groups =
        (fun group shift ->
          match Re.Group.get_opt group shift with
          | None -> Some None
          | Some _ -> Some (t.extract_value_from_captured_groups group (shift + 1)))
    }
  ;;

  module Decimal = struct
    let digit : int t = map Char.digit ~f:(fun c -> Int.of_string (String.of_char c))

    let sign : int t =
      map
        (optional (Char.one_of [ '+'; '-' ]))
        ~f:(fun c ->
          match c with
          | None | Some '+' -> 1
          | Some '-' -> -1
          | Some c ->
            failwith
              [%string "matched a character other than [+, -]: %{String.of_char c}"])
    ;;

    let unsigned = map (capture (repeat ~min:1 (Char.digit |> ignore_m))) ~f:Int.of_string
    let int = map2 sign unsigned ~f:( * )
  end
end

include Body

module Open_on_rhs_intf = struct
  module type S = Parser_intf.S with type 'a t = 'a t
end

include Applicative.Make_let_syntax (Body) (Open_on_rhs_intf) (Body)
