open Lwt.Syntax

let url = "http://www.unicode.org/emoji/charts/emoji-list.html"

let program =
  let* _, body = Cohttp_lwt_unix.Client.get (url |> Uri.of_string) in
  let* html = Cohttp_lwt__Body.to_string body in
  Lwt_io.write_line Lwt_io.stdout html

let () = Lwt_main.run program
