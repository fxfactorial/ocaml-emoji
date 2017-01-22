emojis
------

Single OCaml file containing byte sequences for common `emojis`
sourced from [here](http://apps.timwhitlock.info/emoji/tables/unicode)

```
$ opam install emoji
```

# Development 

Here's the code that generates the `emoji.ml` file

```ocaml
#require "lwt.unix,cohttp.lwt,lambdasoup";;

open Lwt.Infix

let program =
  Cohttp_lwt_unix.Client.get
    ("http://apps.timwhitlock.info/emoji/tables/unicode"
     |> Uri.of_string) >>= fun (_, body) ->
  Cohttp_lwt_body.to_string body >>= fun html ->

  let parsed = Soup.parse html in

  let byte_codes = Soup.select "tbody > tr > td:nth-child(9)" parsed in
  let descriptions = Soup.select "tbody > tr > td:nth-child(10)" parsed in

  let just_innards l =
    l |> List.map (fun l -> l |> Soup.trimmed_texts |> String.concat "")
  in

  let let_names =
    (Soup.to_list descriptions |> just_innards)
    |> List.map (fun s ->
        s |> String.map
          (function ' ' | '-' | '+' -> '_' | c -> Char.lowercase_ascii c)
      )
  in

  let zipped =
    List.combine
      let_names
      (Soup.to_list byte_codes |> just_innards)
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
```

And this simple sanity test:

```ocaml
#require "emoji";;

let () =
  let ar = Emoji.all_emojis |> Array.of_list in
  for i = 0 to Array.length ar - 1 do
    if (i mod 80 = 0) then print_newline ();
    print_string ar.(i);
  done
```

Which for me resulted in:

![](./emojis.png)
