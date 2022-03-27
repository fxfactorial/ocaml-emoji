emojis
------

Single OCaml file containing byte sequences for common `emojis`
sourced from [here](http://www.unicode.org/emoji/charts/emoji-list.html)

```
$ opam install emoji
```

You can see them by printing to the screen, 
ie: `print_endline Emoji.man_facepalming`

And using `ocp-browser` shows the emoji

![](./ocp-browser-emoji.png)

# Development 

First of all pin the package:

```
$ opam pin add emoji /path/to/local/checkout/of/emoji
```

To update the `lib/emoji.ml` this [code](./gencode/main.ml) is
used. Don't forget to run before (re-)installing!

This simple sanity test lets you review your changes:

```ocaml
#require "emoji";;

let () =
  let ar = Emoji.all_emojis |> Array.of_list in
  for i = 0 to Array.length ar - 1 do
    if (i mod 80 = 0) then print_newline ();
    print_string ar.(i);
  done
```

Which for me resulted in:
![](./emojis.png)
