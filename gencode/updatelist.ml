open Lwt.Syntax

let url = "https://www.unicode.org/emoji/charts/full-emoji-list.html"

let url_skin_tones =
  "https://www.unicode.org/emoji/charts/full-emoji-modifiers.html"

let process url outfile =
  let* _, body = Cohttp_lwt_unix.Client.get (url |> Uri.of_string) in
  let* html = Cohttp_lwt__Body.to_string body in
  let* file = Lwt_io.open_file ~mode:Lwt_io.Output outfile in
  Lwt_io.write_line file html

let program =
  let* () = process url "full-emoji-list.html" in
  process url_skin_tones "full-emoji-modifiers.html"

let () = Lwt_main.run program
