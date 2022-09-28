# mal

This is an implementation of the [Mal](https://github.com/kanaka/mal) programming language in Haskell.

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

## Dependencies

A number of libraries are needed to build the project. On Debian/Ubuntu
you can install them with the following command:

```shell
sudo apt-get install libgirepository1.0-dev \
    libwebkit2gtk-4.0-dev \
    libgtksourceview-3.0-dev \
    libreadline-dev \
    libgtk-4-dev
```

## Install

This requires a working installation of Cabal. The easiest way to install it is
through [ghcup](https://www.haskell.org/ghcup/).

```bash
git clone https://github.com/aloussase/mal
cd mal
cabal install
```

## License

MIT
