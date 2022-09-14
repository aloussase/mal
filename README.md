# mal

This is an (almost complete) implementation of the [MAL](https://github.com/kanaka/mal) programming language in Haskell.

```clojure
(fun hello-world (name)
  (println (str "Hello, " name "!")))

(let*
  (name (input "what is your name? "))
  (hello-world name))
```

## TODO

This is a list of additional features not available in the original Mal
language that I am planning to implement, feel free to open a PR if you
want to add any of them:

- [ ] keywords - this are specified in the original mal
- [ ] records - simplified haskell-like records
- [ ] named arguments by default - like in [Jakt](https://github.com/SerenityOS/jakt)

## License

MIT
