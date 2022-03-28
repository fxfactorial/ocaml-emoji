open Lwt.Syntax

type emoji =
  { code_point : string
  ; emoji : string
  ; description : string
  ; name : string
  ; category : string
  ; sub_category : string
  }

let string_escape_hex =
  let fst_code = Char.code '0' in
  let snd_code = Char.code 'a' - 10 in
  let nibble_to_hex_char n =
    if n < 0 || n >= 16 then
      invalid_arg "nibbles must be within the range 0x0 and 0xf"
    else if n < 10 then Char.chr (fst_code + n)
    else Char.chr (snd_code + n)
  in
  fun s ->
    let buf = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        let c = Char.code c in
        Buffer.add_string buf "\\x";
        let n = (c land 0xf0) lsr 4 in
        Buffer.add_char buf (nibble_to_hex_char n);
        let n = c land 0x0f in
        Buffer.add_char buf (nibble_to_hex_char n) )
      s;
    Buffer.contents buf

(* leading ints are illegal in
 * OCaml identifiers so we prepend
 * them with a '_' *)
let wrap_leading_ints s = match s.[0] with '0' .. '9' -> "_" ^ s | _ -> s

let to_legal_ident_char c =
  if not (Uchar.is_char c) then
    (* not a latin1 character, ex: quotation mark (U+2019) in names *)
    "_"
  else
    let uint c = Uchar.of_char c |> Uchar.to_int in
    match Uchar.to_char c with
    | '&' -> "and"
    | '#' -> "hash"
    | '*' -> "star"
    | '-' | ' ' | ':' | '.' | ',' | '(' | ')' -> "_"
    | '!' ->
      (*TODO: use "_" and merge with previous case ? *)
      (* only used for ON! arrow and UP! button *)
      ""
    | '\197' -> "a" (* Å *)
    | '\227' -> "a" (* ã *)
    | '\231' -> "c" (* ç *)
    | '\233' -> "e" (* é *)
    | '\237' -> "i" (* í *)
    | '\241' -> "n" (* piñata !*)
    | '\244' -> "o" (* ô *)
    | ('_' | '\'' | '0' .. '9') as c -> String.make 1 c
    | ('A' .. 'Z' | 'a' .. 'z') as c -> String.make 1 (Char.lowercase_ascii c)
    | c -> failwith (Format.sprintf "unhandled character: '%c'" c)

let deduplicate_underscores s =
  let drop _ s = String.sub s 1 (String.length s - 1) in
  let rec dedup lastwas s =
    if String.length s == 0 then ""
    else
      let c = String.get s 0 in
      if c == '_' then
        if lastwas then dedup true (drop 1 s)
        else String.concat "" [ "_"; dedup true (drop 1 s) ]
      else String.concat "" [ String.make 1 c; dedup false (drop 1 s) ]
  in
  dedup false s

let identifier_of_description s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec list_of_codes acc =
    match Uutf.decode decoder with
    | `Uchar u -> list_of_codes (u :: acc)
    | `End -> List.rev acc
    | `Await -> list_of_codes acc
    | `Malformed _ -> list_of_codes acc
  in
  let codes = list_of_codes [] in
  let legal_chars_list = List.map to_legal_ident_char codes in
  let s = String.concat "" legal_chars_list in
  let s = wrap_leading_ints s in
  let s = deduplicate_underscores s in
  s

let just_innard s = s |> Soup.trimmed_texts |> String.concat ""

let parse_row (l, category, sub_category) el =
  match Soup.select_one "th" el with
  | Some el -> (
    if List.mem "rchars" (Soup.classes el) then
      (* not an emoji row *)
      (l, category, sub_category)
    else
      (* title row *)
      let title =
        match Soup.select_one "a" el with
        | None -> failwith "no link in category row"
        | Some a -> (
          match Soup.attribute "name" a with
          | None -> failwith "no name in category link"
          | Some name -> identifier_of_description @@ String.trim name )
      in
      match Soup.classes el with
      | [] -> failwith "no class name"
      | name :: _l -> (
        match name with
        | "bighead" -> (l, title, "")
        | "mediumhead" -> (l, category, title)
        | _ -> failwith "invalid class name" ) )
  | None -> (
    match Soup.select_one "td.andr > a > img" el with
    | None -> (* not an emoji row *) (l, category, sub_category)
    | Some img ->
      let emoji =
        match Soup.attribute "alt" img with
        | None -> failwith "no alt on emoji img"
        | Some emoji -> emoji
      in
      let description =
        match Soup.select_one "td.name" el with
        | None -> failwith "no description found"
        | Some el -> just_innard el
      in
      (* Recently-added emoji are marked by a ⊛ in the name ⊛_⊛^^ *)
      let description =
        if String.starts_with ~prefix:"⊛" description then
          (* its not 1 *)
          let star_len = String.length "⊛" in
          String.trim
          @@ String.sub description star_len
               (String.length description - star_len)
        else description
      in
      let name = identifier_of_description description in
      let code_point =
        match Soup.select_one "td.code > a" el with
        | None -> failwith "no code_point found"
        | Some el -> just_innard el
      in

      ( { code_point
        ; emoji
        ; description
        ; name
        ; category = "category_" ^ category
        ; sub_category = "sub_category_" ^ sub_category
        }
        :: l
      , category
      , sub_category ) )

let program =
  let* _, body =
    Cohttp_lwt_unix.Client.get
      ("http://www.unicode.org/emoji/charts/emoji-list.html" |> Uri.of_string)
  in
  let* html = Cohttp_lwt__Body.to_string body in
  let parsed = Soup.parse html in
  let table = Soup.children @@ Option.get @@ Soup.select_one "tbody" parsed in
  let init = ([], "", "") in
  let emojis, _last_category, _last_sub_category =
    Soup.fold parse_row init table
  in
  let emojis = List.rev emojis in

  let tbl = Hashtbl.create 512 in
  List.iter
    (fun emoji ->
      match Hashtbl.find_opt tbl emoji.category with
      | None ->
        let sub_cat_tbl = Hashtbl.create 512 in
        let emoji_tbl = Hashtbl.create 512 in
        Hashtbl.add emoji_tbl emoji.name ();
        Hashtbl.add sub_cat_tbl emoji.sub_category emoji_tbl;
        Hashtbl.add tbl emoji.category sub_cat_tbl
      | Some sub_cat_tbl -> (
        match Hashtbl.find_opt sub_cat_tbl emoji.sub_category with
        | None ->
          let emoji_tbl = Hashtbl.create 512 in
          Hashtbl.add emoji_tbl emoji.name ();
          Hashtbl.add sub_cat_tbl emoji.sub_category emoji_tbl
        | Some emoji_tbl -> Hashtbl.add emoji_tbl emoji.name () ) )
    emojis;

  let cat_emojis_list =
    Hashtbl.fold
      (fun category sub_cat_tbl acc ->
        let emojis =
          Hashtbl.fold
            (fun _sub_cat emojis acc ->
              let emojis_list =
                Hashtbl.fold (fun emoji () acc -> emoji :: acc) emojis []
              in
              emojis_list @ acc )
            sub_cat_tbl []
        in
        (category, emojis) :: acc )
      tbl []
  in

  let sub_cat_emojis_list =
    Hashtbl.fold
      (fun _category sub_cat_tbl acc ->
        let sub_categories =
          Hashtbl.fold
            (fun sub_cat emojis acc ->
              let emojis_list =
                Hashtbl.fold (fun emoji () acc -> emoji :: acc) emojis []
              in
              (sub_cat, emojis_list) :: acc )
            sub_cat_tbl []
        in
        sub_categories @ acc )
      tbl []
  in

  let all_names = List.map (fun emoji -> emoji.name) emojis in

  Printf.printf
    "(** All Emojis defined by the Unicode standard, encoded using UTF-8 *)\n";
  List.iter
    (fun e ->
      Printf.printf "\n(** %s (%s): %s *)\nlet %s = \"%s\"\n" e.emoji
        e.code_point e.description
        (identifier_of_description e.description)
        (string_escape_hex e.emoji) )
    emojis;
  List.iter
    (fun (sub_cat, emojis) ->
      Printf.printf "\nlet %s = [|%s|]\n" sub_cat (String.concat ";" emojis) )
    sub_cat_emojis_list;
  List.iter
    (fun (cat, emojis) ->
      Printf.printf "\nlet %s = [|%s|]\n" cat (String.concat ";" emojis) )
    cat_emojis_list;
  Printf.printf
    "\n(** All included emojis in an array *)\nlet all_emojis = [|%s|]"
    (String.concat ";" all_names);
  Lwt.return ()

let () = Lwt_main.run program
