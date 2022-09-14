{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Mal

import           Control.Exception (evaluate)
import           Test.Hspec


parse' = parse (Just $ Mal.MkMalFilename "<tests>")

runWithScope s = do
    scope <- Mal.emptyScope
    run (Just $ Mal.MkMalFilename "<tests>") scope s

coreSpec :: Spec
coreSpec = describe "core lib" $ do
    it "cond works" $ do
        runWithScope "(macroexpand (cond))" `shouldReturn` mkMalNil
        runWithScope "(cond)" `shouldReturn` mkMalNil
        runWithScope "(macroexpand (cond X Y))" `shouldReturn` mkMalList ["if", "X", "Y", mkMalList ["cond"]]
        runWithScope "(cond false 7 false 8 \"else\" 9)" `shouldReturn` mkMalNumber 9
        runWithScope "(cond false 7 false 8 false 9)" `shouldReturn` mkMalNil

    it "fun works" $ do
        runWithScope "(do (fun add (x y) (+ x y)) (add 1 2))" `shouldReturn` mkMalNumber 3

tryCatchSpec :: Spec
tryCatchSpec = describe "try-catch" $ do
    it "throw works" $
        runWithScope "(throw \"err1\")"  `shouldThrow`  anyException

    it "try/catch works" $ do
        runWithScope "(try* 123 (catch* e 456))" `shouldReturn` mkMalNumber 123
        runWithScope "(try* abc (catch* exc (prn \"exc is:\" exc)))" `shouldReturn` MalNil
        runWithScope "(try* (throw \"my exception\") (catch* exc (do (prn \"exc:\" exc) 7)))" `shouldReturn` mkMalNumber 7
        runWithScope "(try* (do (try* \"t1\" (catch* e \"c1\")) (throw \"e1\")) (catch* e \"c2\"))" `shouldReturn` mkMalString "c2"
        runWithScope "(try* (map throw (list \"my err\")) (catch* exc exc))" `shouldReturn` mkMalString "my err"

macrosSpec :: Spec
macrosSpec = describe "macros" $ do
    it "trivial macros work" $ do
        runWithScope "( do (defmacro! one (fn* () 1)) (one))" `shouldReturn` mkMalNumber 1

    it "unless macro works" $ do
        runWithScope "(do (defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a))) (unless false 7 8) )"
            `shouldReturn` mkMalNumber 7
        runWithScope "(do (defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a))) (unless true 7 8) )"
            `shouldReturn` mkMalNumber 8
        runWithScope "(do (defmacro! unless2 (fn* (pred a b) (list 'if (list 'not pred) a b)))  (unless2 false 7 8) )"
            `shouldReturn` mkMalNumber 7

    it "macroexpand works" $ do
        runWithScope "(do (defmacro! one (fn* () 1)) (macroexpand (one)) )"
            `shouldReturn` mkMalNumber 1
        runWithScope "(do (defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a))) (macroexpand (unless PRED A B)) )"
            `shouldReturn` mkMalList ["if", "PRED", "B", "A"]

    it "evauluation of macro result works" $ do
        runWithScope "(do (defmacro! identity (fn* (x) x)) (let* (a 123) (macroexpand (identity a))) )" `shouldReturn` "a"
        runWithScope "(do (defmacro! identity (fn* (x) x)) (let* (a 123) (identity a)) )" `shouldReturn` mkMalNumber 123

    it "macros don't break the empty list" $
        runWithScope "()" `shouldReturn` mkMalList []

    it "macros don't break quasiquote" $
        runWithScope "`(1)" `shouldReturn` mkMalList [mkMalNumber 1]

hashMapSpec :: Spec
hashMapSpec = describe "hash-maps" $ do
    it "hash-map creation works" $ runWithScope "(hash-map \"a\" 1)" `shouldReturn` mkMalMap [mkMalString "a", mkMalNumber 1]
    it "assoc works" $ runWithScope "(assoc {} \"a\" 1)" `shouldReturn` mkMalMap [mkMalString "a", mkMalNumber 1]
    it "get works" $ runWithScope "(get (assoc (assoc {\"a\" 1 } \"b\" 2) \"c\" 3) \"a\")" `shouldReturn` mkMalNumber 1
    it "keys works" $ runWithScope "(keys {\"1\" 1})" `shouldReturn` mkMalList [mkMalString "1"]
    it "vals works" $ runWithScope "(vals {\"1\" 1})" `shouldReturn` mkMalList [mkMalNumber 1]

builtinsSpec :: Spec
builtinsSpec = describe "builtins" $ do
    context "read-string" $ do
        it "can parse a string into a Mal type" $
            runWithScope "(read-string \"(1 2 (3 4) nil)\")"
            `shouldReturn`
            mkMalList [mkMalNumber 1, mkMalNumber 2, mkMalList [mkMalNumber 3, mkMalNumber 4], mkMalNil]

        it "ignores comments" $
            runWithScope "(read-string \"7 ;; comment\")" `shouldReturn` mkMalNumber 7

    context "eval" $ do
        it "can evaluate simple expressions" $
            runWithScope "(eval (read-string \"(+ 2 3)\"))" `shouldReturn` mkMalNumber 5

        it "does not use local environments" $
            runWithScope "(do (def! a 1) (let* (a 2) (eval (read-string \"a\"))) )"
            `shouldReturn`
            mkMalNumber 1

    it "cons works" $ do
        runWithScope "(cons 1 (list))" `shouldReturn` mkMalList [mkMalNumber 1]
        runWithScope "(cons 1 (list 2))" `shouldReturn` mkMalList [mkMalNumber 1, mkMalNumber 2]
        runWithScope "(cons (list 1) (list 2 3))"
            `shouldReturn`
            mkMalList [ mkMalList [mkMalNumber 1]
                        , mkMalNumber 2, mkMalNumber 3
                        ]

    it "concat works" $ do
        runWithScope "(concat)" `shouldReturn` mkMalList []
        runWithScope "(concat (list 1 2))" `shouldReturn` mkMalList [mkMalNumber 1, mkMalNumber 2]
        runWithScope "(concat (list 1 2) (list 3 4))"
            `shouldReturn`
            mkMalList [mkMalNumber 1, mkMalNumber 2, mkMalNumber 3, mkMalNumber 4]
        runWithScope "(concat (concat))" `shouldReturn` mkMalList []
        runWithScope "(concat (list) (list))" `shouldReturn` mkMalList []
        runWithScope "(= () (concat))" `shouldReturn` mkMalBool True

    it "nth works" $ do
        runWithScope "(nth (list 1) 0)" `shouldReturn` mkMalNumber 1
        runWithScope "(nth (list 1 2) 1)" `shouldReturn` mkMalNumber 2
        runWithScope "(nth (list 1 2 nil) 2)" `shouldReturn` mkMalNil
        runWithScope "(def! x (nth (list 1 2) 2))" `shouldThrow` anyException

    it "first works" $ do
        runWithScope "(first (list))" `shouldReturn` mkMalNil
        runWithScope "(first (list 6))" `shouldReturn` mkMalNumber 6
        runWithScope "(first (list 7 8 9))" `shouldReturn` mkMalNumber 7

    it "rest works" $ do
        runWithScope "(rest (list))" `shouldReturn` mkMalList []
        runWithScope "(rest (list 6))" `shouldReturn` mkMalList []
        runWithScope "(rest (list 7 8 9))" `shouldReturn` mkMalList [mkMalNumber 8, mkMalNumber 9]

    it "map works" $ do
        runWithScope "(do (def! nums (list 1 2 3)) (def! double (fn* (a) (* 2 a))) (map double nums)  )"
            `shouldReturn` mkMalList [mkMalNumber 2, mkMalNumber 4, mkMalNumber 6]
        runWithScope "(map (fn* (x) (symbol? x)) (list 1 (quote two) \"three\"))"
            `shouldReturn` mkMalList [mkMalBool False, mkMalBool True, mkMalBool False]
        runWithScope "(= () (map str ()))" `shouldReturn` mkMalBool True

evaluatorSpec :: Spec
evaluatorSpec = describe "Mal.run" $ do
    context "arithmetic" $ do
        it "can evaluate simple arithmetic" $ do
            runWithScope "(+ 1 2)" `shouldReturn` mkMalNumber 3
            runWithScope "(* 1 2)" `shouldReturn` mkMalNumber 2
            runWithScope "(/ 2 1)" `shouldReturn` mkMalNumber 2
            runWithScope "(- 2 1)" `shouldReturn` mkMalNumber 1

        it "can evalute expressions nested in vectors" $ do
            runWithScope "[(+ 1 2) (* 2 3 (+ 3 2))]" `shouldReturn` mkMalVector [mkMalNumber 3, mkMalNumber 30]

        it "can evaluate expression nested in hash maps" $ do
            runWithScope "{hello (* 3 2) world (+ 5 (* 2 1))}"
                `shouldReturn` mkMalMap ["hello", mkMalNumber 6, "world", mkMalNumber 7]

    context "environment" $ do
        context "def!" $ do
            it "can bind simple expressions" $
                runWithScope "(do (def! x 10) (+ x 10))" `shouldReturn` mkMalNumber 20

            it "shadows existing bindings" $
                runWithScope "(do (def! x 12) (def! x 10) x)" `shouldReturn` mkMalNumber 10

            it "binds variables at the global scope" $
                runWithScope "(do (let* () (def! x 10)) x)" `shouldReturn` mkMalNumber 10

        context "let*" $ do
            it "can bind simple expressions" $
                runWithScope "(let* (x 10) (+ x 4))" `shouldReturn` mkMalNumber 14

            it "works with nested bindings" $ do
                runWithScope "(let* (x (let* (y 10) (+ y 12))) (+ x 12))" `shouldReturn` mkMalNumber 34
                runWithScope "(let* (x 10) (let* (y 2) (+ x y)))" `shouldReturn` mkMalNumber 12

            it "can bind builtin functions" $
                runWithScope "(let* (add +) (add 1 2))" `shouldReturn` mkMalNumber 3

            it "does not let bindings escape their scope" $
                runWithScope "(do (let* (x 10)) x)" `shouldThrow` anyException

            it "works with multiple expresssion in the body" $
                runWithScope "(let* (x 10) x (+ x 1))" `shouldReturn` mkMalNumber 11

    context "control structures" $ do
        context "do" $ do
            it "can evaluate many expressions" $ do
                runWithScope "(do (+ 1 2) 12 \"hello\" true false nil)" `shouldReturn` mkMalNil

            it "can be nested" $ do
                runWithScope "(do (def! x 10) (do (+ x 22)))" `shouldReturn` mkMalNumber 32

        context "if" $ do
            it "works without an else branch" $ do
                runWithScope "(if true 12)" `shouldReturn` mkMalNumber 12
                runWithScope "(if false 12)" `shouldReturn` mkMalNil

            it "works with an else branch" $ do
                runWithScope "(if true 12 43)" `shouldReturn` mkMalNumber 12
                runWithScope "(if false 12 43)" `shouldReturn` mkMalNumber 43

            it "works with complex expressions" $ do
                runWithScope "(if (+ 1 2) (do (def! x 10) (+ x 10)))" `shouldReturn` mkMalNumber 20
                runWithScope "(if nil nil (do (let* (x (+ 1 2)) x)))" `shouldReturn` mkMalNumber 3

        context "fn*" $ do
            it "simple functions work" $
                runWithScope "((fn* (x y) (+ x y)) 10 10)" `shouldReturn` mkMalNumber 20

            it "works with def!" $
                runWithScope "(do (def! add (fn* (x y) (+ x y))) (add 2 3))" `shouldReturn` mkMalNumber 5

            it "recursive functions work" $ do
                runWithScope "(do (def! sumdown (fn* (N) (if (> N 0) (+ N (sumdown  (- N 1))) 0))) (sumdown 6))"
                    `shouldReturn`
                    mkMalNumber 21
                runWithScope "(do (def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2))))))) (fib 4))"
                    `shouldReturn`
                    mkMalNumber 5

            it "recursive function in environment works" $ do
                runWithScope "(let* (f (fn* () x) x 3) (f))" `shouldReturn` mkMalNumber 3
                runWithScope "(let* (f (fn* (n) (if (= n 0) 0 (g (- n 1)))) g (fn* (n) (f n))) (f 2))"
                    `shouldReturn` mkMalNumber 0

            it "works with rest params" $ do
                runWithScope "( (fn* (& more) (count more)) 1 2 3)" `shouldReturn` mkMalNumber 3
                runWithScope "( (fn* (& more) (list? more)) 1 2 3)" `shouldReturn` mkMalBool True
                runWithScope "( (fn* (& more) (count more)) )" `shouldReturn` mkMalNumber 0
                runWithScope "( (fn* (& more) (list? more)) )" `shouldReturn` mkMalBool True
                runWithScope "( (fn* (a & more) (count more)) 1 2 3)" `shouldReturn` mkMalNumber 2
                runWithScope "( (fn* (a & more) (count more)) 1)" `shouldReturn` mkMalNumber 0
                runWithScope "( (fn* (a & more) (list? more)) 1)" `shouldReturn` mkMalBool True

            it "TCO works correctly" $ do
                runWithScope "(do (def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc))))) (sum2 10000 0) )"
                    `shouldReturn` mkMalNumber 50005000
                runWithScope
                    (mconcat ["(do "
                             , "(def! foo (fn* (n) (if (= n 0) 0 (bar (- n 1)))))"
                             , "(def! bar (fn* (n) (if (= n 0) 0 (foo (- n 1)))))"
                             , "(foo 10000)"
                             , ")"]) `shouldReturn` mkMalNumber 0

        context "quote" $ do
            it "works" $ do
                runWithScope "(quote 7)" `shouldReturn` mkMalNumber 7
                runWithScope "(quote (1 2 3))"
                    `shouldReturn`
                    mkMalList [mkMalNumber 1, mkMalNumber 2, mkMalNumber 3]
                runWithScope "(quote (1 2 (3 4)))"
                    `shouldReturn`
                    mkMalList [ mkMalNumber 1, mkMalNumber 2
                              , mkMalList [mkMalNumber 3, mkMalNumber 4]
                              ]

        context "quasiquote" $ do
            it "works with simple expressions" $ do
                runWithScope "(quasiquote nil)" `shouldReturn` mkMalNil
                runWithScope "(quasiquote 7)" `shouldReturn` mkMalNumber 7
                runWithScope "(quasiquote {\"a\" b})"
                    `shouldReturn` mkMalMap [mkMalString "a", "b"]

            it "works with lists" $ do
                runWithScope "(quasiquote ())" `shouldReturn` mkMalList []
                runWithScope "(quasiquote (1 2 3))" `shouldReturn` mkMalList (map mkMalNumber [1..3])
                runWithScope "(quasiquote (nil))" `shouldReturn` mkMalList [mkMalNil]
                runWithScope "(quasiquote (1 () 2))" `shouldReturn` mkMalList [mkMalNumber 1, mkMalList [], mkMalNumber 2]

        context "unquote" $ do
            it "works" $ do
                runWithScope "(quasiquote (unquote 7))" `shouldReturn` mkMalNumber 7
                runWithScope "(do (def! a 8) (quasiquote (unquote a)))" `shouldReturn` mkMalNumber 8
                runWithScope "(do (def! a 8) (quasiquote (1 a 3)) )"
                    `shouldReturn` mkMalList [mkMalNumber 1, "a", mkMalNumber 3]
                runWithScope "(quasiquote ((unquote 1) (unquote 2)))" `shouldReturn` mkMalList [mkMalNumber 1, mkMalNumber 2]

            it "works in let*" $
                runWithScope "(let* (x 0) (quasiquote (unquote x)))" `shouldReturn` mkMalNumber 0

        context "splice-unquote" $ do
            it "works" $ do
                runWithScope "(do (def! c (quote (1 \"b\" \"d\"))) (quasiquote (1 (splice-unquote c) 3)) )"
                    `shouldReturn`
                    mkMalList [ mkMalNumber 1, mkMalNumber 1, mkMalString "b", mkMalString "d", mkMalNumber 3]
                runWithScope "(do (def! c (quote (1 \"b\" \"d\"))) (quasiquote ((splice-unquote c) (splice-unquote c))) )"
                    `shouldReturn`
                    mkMalList (take 6 $ cycle [ mkMalNumber 1, mkMalString "b", mkMalString "d"])

parserSpec :: Spec
parserSpec = describe "Mal.parse" $ do
    it "can parse atoms" $ do
        parse' "1" `shouldBe`  mkMalNumber 1
        parse' "\"Hello\"" `shouldBe` mkMalString "Hello"
        parse' "true"  `shouldBe` mkMalBool True
        parse' "false"  `shouldBe` mkMalBool False
        parse' "nil" `shouldBe` mkMalNil

    it "can parse empty lists" $ parse' "()" `shouldBe`  mkMalList []

    it "can parse non-empty lists" $ parse' "(1 \"Hello\" true false nil some-symbol)"
        `shouldBe`
         mkMalList [ mkMalNumber 1
                   , mkMalString "Hello"
                   , mkMalBool True
                   , mkMalBool False
                   , mkMalNil
                   , "some-symbol"
                   ]

    it "can parse nested lists" $ parse' "(((1 (\"hello\" true))))"
        `shouldBe`
         mkMalList [ mkMalList [ mkMalList [ mkMalNumber 1
                                           , mkMalList [mkMalString "hello", mkMalBool True]
                                           ]
                               ]
                   ]

    it "can parse maps" $
        parse' "{\"hello\" world}" `shouldBe`  mkMalMap [mkMalString "hello", "world"]

    it "ignores extra elements when parsing maps" $
        parse' "{\"hello\" world 69}" `shouldBe`  mkMalMap [mkMalString "hello", "world"]

    it "fails on unterminated string literals" $
        evaluate (parse' "\"Hello") `shouldThrow` anyException

main :: IO ()
main = hspec  $ do
    parserSpec
    evaluatorSpec
    builtinsSpec
    macrosSpec
    hashMapSpec
    tryCatchSpec
    coreSpec
