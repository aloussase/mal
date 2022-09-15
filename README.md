# mal

This is an (almost complete) implementation of the [MAL](https://github.com/kanaka/mal) programming language in Haskell.

## Examples

### Hello, World!

```clojure
(fun make-person (name)
  {:name name})

(fun hello-world (person)
  (println (str "Hello, " (get person "name") "!")))

(let*
  (name (input "what is your name? "))
  (hello-world (make-person :name name)))
```

### FizzBuzz

```clojure
(fun fizzbuzz (from to)
  (when (< from to)
    (cond
      (= 0 (mod from 15)) (println "FizzBuzz")
      (= 0 (mod from 3))  (println "Fizz")
      (= 0 (mod from 5))  (println "Buzz")
      :else (println from))
    (fizzbuzz (inc from) to)))

(if (< (count *ARGV*) 2)
  (println "Usage: fizzbuzz <n>")
  (fizzbuzz 1 (read-string (nth *ARGV* 1))))
```

For a more complex examples (such a command line todo app), see the `examples` directory.

## Install

This requires a working installation of Cabal. The easiest way to install it is
through [ghcup](https://www.haskell.org/ghcup/).

```bash
git clone https://github.com/aloussase/mal
cd mal
cabal install
```

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
