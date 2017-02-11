#require "lwt.unix,cohttp.lwt,lambdasoup,uutf";;

open Lwt.Infix

module Bytes = struct
  include Bytes
  let map_to_list (f : char -> 'b) (s :bytes) =
    let rec map_tail_reverse f s i acc =
      if i >= 0
    then map_tail_reverse f s (i - 1) (f s.[i] :: acc)
    else acc
    in map_tail_reverse f s (String.length s - 1) []
end

let rec hex_escape_sequence c =
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

let rec to_legal_ident_char c =
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

let rec deduplicate_underscores s =
  let drop n s = String.sub s 1 (String.length s - 1) in
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

let program =
  Cohttp_lwt_unix.Client.get
    ("http://www.unicode.org/emoji/charts/emoji-list.html"
     |> Uri.of_string) >>= fun (_, body) ->
  Cohttp_lwt_body.to_string body >>= fun html ->

  let parsed = Soup.parse html in

  let just_innards l =
    l |> List.map (fun l -> l |> Soup.trimmed_texts |> String.concat "")
  in

  let emoji_chars = Soup.select "tbody > tr > td.andr > a > img" parsed
                   |> Soup.to_list |> List.map (fun el ->
                       Soup.attribute "alt" el |> fun (Some x) -> x
                       |> Bytes.of_string |> Bytes.map_to_list hex_escape_sequence
                       |> String.concat "")
  in
  let descriptions = Soup.select "tbody > tr > td.name" parsed in

  let let_names =
    (Soup.to_list descriptions |> just_innards)
    |> List.map identifier_of_description
  in

  let zipped =
    List.combine
      let_names
      emoji_chars
  in
  Lwt_io.open_file ~mode:Lwt_io.Output "lib/emoji.ml" >>= fun output ->
  zipped |> Lwt_list.iter_s (fun (name, bytes) ->
      Printf.sprintf "let %s = \"%s\"" name bytes
      |> Lwt_io.write_line output
    ) >>= fun () ->
  Printf.sprintf "let all_emojis = [%s]" (String.concat ";" let_names)
  |> Lwt_io.write_line output

let () =
  Lwt_main.run program
