let print_emojis emojis =
  Array.iter (fun emoji -> print_string emoji) emojis;
  print_newline ()

let () =
  print_string "All emojis:\n";
  print_emojis Emoji.all_emojis;
  print_string "Animal & Nature:\n";
  print_emojis Emoji.category_animals_and_nature;
  print_string "Animal-reptile:\n";
  print_emojis Emoji.sub_category_animal_reptile;
  print_string "melting face:\n";
  print_string Emoji.melting_face;
  print_newline ()
