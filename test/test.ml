(* from https://www.unicode.org/emoji/charts/emoji-counts.html *)
let () =
  assert (Array.length Emoji.category_smileys_and_emotion = 169);
  assert (Array.length Emoji.category_people_and_body = 2261);
  assert (Array.length Emoji.category_animals_and_nature = 159);
  assert (Array.length Emoji.category_symbols = 224);
  assert (Array.length Emoji.all_emojis = 3790)
