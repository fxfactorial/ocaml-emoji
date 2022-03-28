let () =
  let ar = Emoji.all_emojis |> Array.of_list in
  for i = 0 to Array.length ar - 1 do
    if i mod 80 = 0 then print_newline ();
    print_string ar.(i)
  done
