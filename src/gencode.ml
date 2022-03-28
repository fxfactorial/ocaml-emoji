open Lwt.Infix

type emoji =
  { code_point : string
  ; emoji : string
  ; description : string
  ; name : string
  ; category : string
  ; sub_category : string
  }

module Bytes = struct
  include Bytes

  let map_to_list (f : char -> 'b) (s : t) =
    let rec map_tail_reverse f s i acc =
      if i >= 0 then map_tail_reverse f s (i - 1) (f (Bytes.get s i) :: acc)
      else acc
    in
    map_tail_reverse f s (Bytes.length s - 1) []
end

let hex_escape_sequence c =
  let nibble_to_hex_char n =
    match n with
    | n when n >= 0 && n < 10 -> Char.chr (Char.code '0' + n)
    | n when n >= 10 && n < 16 -> Char.chr (Char.code 'a' + (n - 10))
    | _ ->
      raise (Invalid_argument "Nibbles must be within the range of 0x0 and 0xf")
  in
  let i = Char.code c in
  let n1 = (i land 0xf0) lsr 4 in
  let n2 = i land 0x0f in
  let initfun = function
    | 0 -> nibble_to_hex_char n1
    | 1 -> nibble_to_hex_char n2
    | _ -> raise (Invalid_argument "Only indices between 0 and 1 are allowed")
  in
  String.concat "" [ "\\x"; String.init 2 initfun ]

(* leading ints are illegal in
 * OCaml identifiers so we prepend
 * them with a '_' *)
let wrap_leading_ints s =
  let i = Char.code (String.get s 0) in
  if i >= Char.code '0' && i <= Char.code '9' then String.concat "" [ "_"; s ]
  else s

let to_legal_ident_char c =
  let uint c = Uchar.of_char c |> Uchar.to_int in
  match Uchar.to_int c with
  | i when i = uint '&' -> "and"
  | i when (i = uint '_' || i = uint '\'') || (i >= uint '0' && i <= uint '9')
    ->
    String.make 1 (Uchar.to_char c)
  | i when (i >= uint 'A' && i <= uint 'Z') || (i >= uint 'a' && i <= uint 'z')
    ->
    String.make 1 (Char.lowercase_ascii (Uchar.to_char c))
  | _ -> "_"

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
  list_of_codes [] |> List.map to_legal_ident_char |> fun l ->
  String.concat "" l |> wrap_leading_ints |> deduplicate_underscores

let emoji_bytes el =
  Bytes.of_string el
  |> Bytes.map_to_list hex_escape_sequence
  |> String.concat ""

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
    | Some emoji ->
      let emoji =
        match Soup.attribute "alt" emoji with
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
  Cohttp_lwt_unix.Client.get
    ("http://www.unicode.org/emoji/charts/emoji-list.html" |> Uri.of_string)
  >>= fun (_, body) ->
  Cohttp_lwt__Body.to_string body >>= fun html ->
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

  Lwt_io.stdout |> fun output ->
  Printf.sprintf
    "(** All Emojis defined by the Unicode standard, encoded using UTF-8 *)\n"
  |> Lwt_io.write_line output
  >>= fun () ->
  emojis
  |> Lwt_list.iter_s (fun e ->
         Printf.sprintf "(** %s (%s): %s *)\nlet %s = \"%s\"\n" e.emoji
           e.code_point e.description
           (identifier_of_description e.description)
           (emoji_bytes e.emoji)
         |> Lwt_io.write_line output )
  >>= fun () ->
  sub_cat_emojis_list
  |> Lwt_list.iter_s (fun (sub_cat, emojis) ->
         Printf.sprintf "\nlet %s = [|%s|]" sub_cat (String.concat ";" emojis)
         |> Lwt_io.write_line output )
  >>= fun () ->
  cat_emojis_list
  |> Lwt_list.iter_s (fun (cat, emojis) ->
         Printf.sprintf "\nlet %s = [|%s|]" cat (String.concat ";" emojis)
         |> Lwt_io.write_line output )
  >>= fun () ->
  Printf.sprintf
    "(** All included emojis in an array *)\nlet all_emojis = [|%s|]"
    (String.concat ";" all_names)
  |> Lwt_io.write_line output

let () = Lwt_main.run program
