open Lwt.Infix

type emoji = {
  code_point  : string;
  emoji       : string;
  description : string;
  name        : string;
}

let clean_up_description =
  List.fold_right
    (fun (re, replacement) -> Str.global_replace (Str.regexp re) replacement)
    ["⊛ ", ""] (* remove the indicator of new Unicode emojis *)

let rec emojis_zip codes emojis descs names =
  match codes, emojis, descs, names with
  | [], [], [], [] -> []
  | code_point :: codes, emoji :: emojis, description :: descs, name :: names ->
    {code_point; emoji; description; name} :: emojis_zip codes emojis descs names
  | _ -> invalid_arg "Lists must be of the same length"

(* leading ints are illegal in OCaml identifiers *)
let rewrite_leading_digit =
  List.fold_right
    (fun (re, replacement) -> Str.global_replace (Str.regexp re) replacement)
    ["^1st", "first"; "^2nd", "second"; "^3rd", "third"]

let char_to_legal_ident_char =
  function
  | '&' -> "and"
  | '*' -> "asterisk"
  | '#' -> "number_sign"
  | ('_' | '\'' | '0' .. '9') as c -> String.make 1 c
  | ('a' .. 'z' | 'A' .. 'Z') as c -> String.make 1 (Char.lowercase_ascii c)
  | '\xc5' (* Å *) -> "aa" (* Åland Islands *)
  | '\xe3' (* ã *) -> "a" (* São Tomé & Príncipe *)
  | '\xe7' (* ç *) -> "c" (* Curaçao *)
  | '\xe9' (* é *) -> "e" (* St. Barthélemy, Réunion, São Tomé & Príncipe, *)
  | '\xed' (* í *) -> "i" (* São Tomé & Príncipe *)
  | '\xf1' (* ñ *) -> "n" (* piñata *)
  | '\xf4' (* ô *) -> "o" (* Côte d’Ivoire *)
  | ' ' | '!' | '(' | ')' | ',' | '-' | '.' | ':' -> "_"
  | c -> invalid_arg (Printf.sprintf "latin1_to_legal_ident_char: unexpected code point %#x; please update main.ml" (Char.code c))

let to_legal_ident_char c =
  if Uchar.is_char c
  then char_to_legal_ident_char (Uchar.to_char c)
  else
    match Uchar.to_int c with
    | 0x2019 (* ’ *) -> "'" (* o’clock, man’s, woman’s, worker’s, Côte d’Ivoire, etc. *)
    | 0x201c (* “ *) | 0x201d (* ” *) -> "_"
    | i -> invalid_arg (Printf.sprintf "to_legal_ident_char: unexpected code point %#x; please update main.ml" i)

let clean_up_underscores s =
  s
  |> Str.global_replace (Str.regexp "_+") "_"
  |> Str.global_replace (Str.regexp "^_+") ""
  |> Str.global_replace (Str.regexp "_+$") ""

let identifier_of_description s =
  let open Uchar in
  let rec list_of_codes acc i =
    if i >= String.length s
    then List.rev acc
    else
      let decoder = String.get_utf_8_uchar s i in
      list_of_codes (utf_decode_uchar decoder :: acc) (i + utf_decode_length decoder)
  in
  list_of_codes [] 0
  |> List.map to_legal_ident_char |> String.concat ""
  |> rewrite_leading_digit
  |> clean_up_underscores

let extract_data url =
  Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (_, body) ->
  Cohttp_lwt__Body.to_string body >>= fun html ->

  let parsed = Soup.parse html in

  let extract_rows cls =
    Soup.select (Printf.sprintf "tbody > tr > td.%s" cls) parsed
    |> Soup.to_list
    |> List.map (fun l -> l |> Soup.trimmed_texts |> String.concat "")
  in

  let emojis = extract_rows "chars" in
  let descriptions = extract_rows "name" |> List.map clean_up_description in
  let code_points = extract_rows "code" in

  let let_names = List.map identifier_of_description descriptions in
  Lwt.return (emojis_zip code_points emojis descriptions let_names, let_names)

(*"http://www.unicode.org/emoji/charts/full-emoji-list.html" *)
let program =
  extract_data "https://www.unicode.org/emoji/charts/full-emoji-list.html" >>= fun (emojis, emoji_let_names) ->
  extract_data "https://www.unicode.org/emoji/charts/full-emoji-modifiers.html" >>= fun (skin_tones, skin_tones_let_names) ->

  Lwt_io.open_file ~mode:Lwt_io.Output "lib/emoji.ml" >>= fun output ->
  Lwt_io.write_line output "(** All Emojis defined by the Unicode standard, encoded using UTF-8 *)"
  >>= fun () ->
  (emojis @ skin_tones) |> Lwt_list.iter_s (fun e ->
      Printf.sprintf "\n(** %s (%s): %s *)\nlet %s = \"%s\""
        e.emoji e.code_point e.description
        (identifier_of_description e.description) (String.escaped e.emoji)
      |> Lwt_io.write_line output
    ) >>= fun () ->
  Printf.sprintf "\n(** All included emojis in a list *)\n\
                  let all_emojis = [%s]" (String.concat ";" @@ emoji_let_names @ skin_tones_let_names)
  |> Lwt_io.write_line output >>= fun () ->
  Printf.sprintf "\n(** All included emojis without modifiers in a list *)\n\
                  let all_emojis_without_skin_tones = [%s]" (String.concat ";" @@ emoji_let_names)
  |> Lwt_io.write_line output

let () =
  Lwt_main.run program
