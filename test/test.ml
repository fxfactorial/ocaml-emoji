let print_emojis emojis =
  Array.iteri
    (fun i emoji ->
      if i mod 80 = 0 then print_newline ();
      print_string emoji )
    emojis;
  print_newline ()

let () =
  print_emojis Emoji.all_emojis;
  print_string "Animal & Nature:";
  print_emojis Emoji.category_animals_and_nature;
  print_string "Animal-reptile:";
  print_emojis Emoji.sub_category_animal_reptile;
  print_string "melting face:";
  print_newline ();
  print_string Emoji.melting_face;
  print_newline ()
