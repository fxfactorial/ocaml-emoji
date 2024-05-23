emoji
------

OCaml library providing byte sequences of all the Unicode emoji 
characters and sequences sourced from [here](http://www.unicode.org/emoji/charts/emoji-list.html )

```
$ opam install emoji
```

You can see them by printing to the screen: 
```ocaml
# Emoji.troll;;
- : string = "🧌"
```

You can also get all emojis from the same category or subcategory:
```ocaml
# let best_animals = Emoji.sub_category_animal_reptile;;
val best_animals : string array =
  [|"🐊"; "🐉"; "🐲"; "🦎"; "🦕"; "🐍"; "🦖"; "🐢"|]
```

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

