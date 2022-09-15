# mal

This is an (almost complete) implementation of the [MAL](https://github.com/kanaka/mal) programming language in Haskell.

```clojure
(fun make-person (name)
  {"name" name})

(fun hello-world (person)
  (println (str "Hello, " (get person "name") "!")))

(let*
  (name (input "what is your name? "))
  (hello-world (make-person :name name)))
```

For a more complex examples (such a command line todo app), see the `examples` directory.

## TODO

This is a list of additional features not available in the original Mal
language that I am planning to implement, feel free to open a PR if you
want to add any of them:

- [x] keywords - this are specified in the original mal
- [ ] records - simplified haskell-like records
- [x] ~named arguments by default - like in [Jakt](https://github.com/SerenityOS/jakt)~
  - optional positional named arguments were added
- [ ] better error messages in the interpreter

## License

MIT
