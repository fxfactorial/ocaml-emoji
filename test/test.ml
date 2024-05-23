let () =
  (* from https://www.unicode.org/emoji/charts/emoji-counts.html *)
  assert (Array.length Emoji.category_animals_and_nature = 153);
  assert (Array.length Emoji.category_smileys_and_emotion = 168);
  assert (Array.length Emoji.category_symbols = 223);
  assert (Array.length Emoji.category_people_and_body = 2260);
  assert (Array.length Emoji.all_emojis = 3782)

let print_emojis emojis =
  Array.iter print_string emojis;
  print_newline ()

let () =
  print_string "All emojis:\n";
  print_emojis Emoji.all_emojis;
  print_string "Animal & Nature:\n";
  print_emojis Emoji.category_animals_and_nature;
  print_string "Animal-reptile:\n";
  print_emojis Emoji.sub_category_animal_reptile;
  Printf.printf "melting face:\n%s\n" Emoji.melting_face;
  Printf.printf "head shaking horizontally:\n%s\n"
    Emoji.head_shaking_horizontally;
  Printf.printf "head shaking vertically:\n%s\n" Emoji.head_shaking_vertically;
  ()
