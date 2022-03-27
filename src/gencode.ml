open Lwt.Infix

type emoji = {
  code_point  : string;
  emoji       : string;
  description : string;
  name        : string;
}

let rec emojis_list codes emojis descs names =
  if not (List.length codes = List.length emojis &&
     List.length emojis = List.length descs &&
     List.length descs = List.length names)
  then raise (Invalid_argument "Lists must be of the same length")
  else if List.length codes = 0
    then []
    else { code_point = List.hd codes; emoji = List.hd emojis;
           description = List.hd descs; name = List.hd names; }
          :: (emojis_list (List.tl codes) (List.tl emojis)
                          (List.tl descs) (List.tl names))

module Bytes = struct
  include Bytes
  let map_to_list (f : char -> 'b) (s : t) =
    let rec map_tail_reverse f s i acc =
      if i >= 0
    then map_tail_reverse f s (i - 1) (f (Bytes.get s i) :: acc)
    else acc
    in map_tail_reverse f s (Bytes.length s - 1) []
end

let hex_escape_sequence c =
  let nibble_to_hex_char n = match n with
    | n when n >= 0  && n < 10 -> Char.chr (Char.code '0' + n)
    | n when n >= 10 && n < 16 -> Char.chr (Char.code 'a' + (n - 10))
    | _ -> raise (Invalid_argument "Nibbles must be within the range of 0x0 and 0xf")
  in
  let i  = Char.code c in
  let n1 = (i land 0xf0) lsr 4 in
  let n2 = (i land 0x0f) in
  let initfun = function
    | 0 -> nibble_to_hex_char n1
    | 1 -> nibble_to_hex_char n2
    | _ -> raise (Invalid_argument "Only indices between 0 and 1 are allowed")
  in String.concat "" ["\\x"; String.init 2 initfun; ]

(* leading ints are illegal in
 * OCaml identifiers so we prepend
 * them with a '_' *)
let wrap_leading_ints s =
  let i = Char.code (String.get s 0) in
  if i >= (Char.code '0') && i <= (Char.code '9')
  then String.concat "" ["_"; s]
  else s

let to_legal_ident_char c =
  let uint c = Uchar.of_char c |> Uchar.to_int in
  match (Uchar.to_int c) with
  | i when i = (uint '&') -> "and"
  | i when (i = (uint '_') || i = (uint '\''))
           || (i >= (uint '0') && i <= (uint '9'))
    -> String.make 1 (Uchar.to_char c)
  | i when (i >= (uint 'A') && i <= (uint 'Z'))
           || (i >= (uint 'a') && i <= (uint 'z'))
    -> String.make 1 (Char.lowercase_ascii (Uchar.to_char c))
  | _ -> "_"

let deduplicate_underscores s =
  let drop _ s = String.sub s 1 (String.length s - 1) in
  let rec dedup lastwas s =
    if String.length s == 0 then ""
    else let c = String.get s 0 in
         if c == '_'
         then if lastwas
           then dedup true (drop 1 s)
           else String.concat "" ["_"; dedup true (drop 1 s) ]
         else String.concat "" [ String.make 1 c; dedup false (drop 1 s) ]
  in dedup false s

let identifier_of_description s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec list_of_codes acc =
    match Uutf.decode decoder with
    | `Uchar u -> list_of_codes (u :: acc)
    | `End -> List.rev acc
    | `Await -> list_of_codes acc
    | `Malformed _ -> list_of_codes acc
  in
  list_of_codes [] |> List.map to_legal_ident_char
    |> fun l -> String.concat "" l
    |> wrap_leading_ints |> deduplicate_underscores


let emoji_bytes el =
  Bytes.of_string el |> Bytes.map_to_list hex_escape_sequence |> String.concat ""

let program =
  Cohttp_lwt_unix.Client.get
    ("http://www.unicode.org/emoji/charts/emoji-list.html"
     |> Uri.of_string) >>= fun (_, body) ->
  Cohttp_lwt__Body.to_string body >>= fun html ->

  let parsed = Soup.parse html in

  let just_innards l =
    l |> List.map (fun l -> l |> Soup.trimmed_texts |> String.concat "")
  in

  let emojis = Soup.select "tbody > tr > td.andr > a > img" parsed
               |> Soup.to_list
               |> List.map (fun el ->
                   Soup.attribute "alt" el |> (function | Some x -> x | None -> assert false))
  in

  let descriptions = Soup.select "tbody > tr > td.name" parsed
                     |> Soup.to_list |> just_innards in


  let code_points = Soup.select "tbody > tr > td.code > a" parsed
                    |> Soup.to_list |> just_innards
  in


  let let_names = List.map identifier_of_description descriptions in
  let zipped = emojis_list code_points emojis descriptions let_names in

  Lwt_io.open_file ~mode:Lwt_io.Output "emoji.ml" >>= fun output ->
    Printf.sprintf "(** All Emojis defined by the \
    Unicode standard, encoded using UTF-8 *)\n" |> Lwt_io.write_line output
    >>= fun () ->
      zipped |> Lwt_list.iter_s (fun e ->
        Printf.sprintf "(** %s (%s): %s *)\nlet %s = \"%s\"\n"
        e.emoji e.code_point e.description
        (identifier_of_description e.description) (emoji_bytes e.emoji)
               |> Lwt_io.write_line output
    ) >>= fun () ->
      Printf.sprintf "(** All included emojis in a list *)\n\
      let all_emojis = [%s]" (String.concat ";" let_names)
               |> Lwt_io.write_line output

let () =
  Lwt_main.run program
