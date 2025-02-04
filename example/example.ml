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
  Printf.printf "face with bags under eyes: %s\n"
    Emoji.face_with_bags_under_eyes;
  Printf.printf "leafless tree: %s\n" Emoji.leafless_tree;
  Printf.printf "root vegetable: %s\n" Emoji.root_vegetable;
  Printf.printf "splatter: %s\n" Emoji.splatter;
  ()
