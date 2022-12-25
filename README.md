emoji
------

OCaml library providing byte sequences of all the Unicode emoji 
characters and sequences sourced from [here](http://www.unicode.org/emoji/charts/emoji-list.html )

```
$ opam install emoji
```

You can see them by printing to the screen: 
```ocaml
print_endline Emoji.troll
```
🧌

You can also get all emojis from the same category or subcategory:
```ocaml
let best_animals = Emoji.sub_category_animal_reptile in
Array.iter print_string best_animals;
```
🐉🐊🦎🦖🦕🐢🐲🐍

Using `ocp-browser` shows the emoji

![ocp-browser screenshot](./ocp-browser-emoji.png)

# Development 

generate emoji.ml with 
```
$ dune exec gencode/updatelist.exe && dune exec gencode/gencode.exe > src/emoji.ml
```
test with 
```
$ dune runtest
```
Which for me resulted in:
![emojis_screenshot](./emojis.png)

