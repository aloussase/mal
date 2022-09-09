module Main (main) where

import           Mal

import           Control.Exception (evaluate)
import           Test.Hspec


runWithScope s = do
    scope <- Mal.emptyScope
    run scope s

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
                `shouldReturn` mkMalMap [mkMalSymbol "hello", mkMalNumber 6, mkMalSymbol "world", mkMalNumber 7]

    context "environment" $ do
        context "def!" $ do
            it "can bind simple expressions" $
                runWithScope "(do (def! x 10) (+ x 10))" `shouldReturn` mkMalNumber 20

            it "shadows existing bindings" $
                runWithScope "(do (def! x 12) (def! x 10) x)" `shouldReturn` mkMalNumber 10

            it "binds variables at the current scope only" $
                runWithScope "(do (let* () (def! x 10)) x)" `shouldThrow` anyException

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

parserSpec :: Spec
parserSpec = describe "Mal.parse" $ do
    it "can parse atoms" $ do
        parse "1" `shouldBe`  mkMalNumber 1
        parse "\"Hello\"" `shouldBe` mkMalString "Hello"
        parse "true"  `shouldBe` mkMalBool True
        parse "false"  `shouldBe` mkMalBool False
        parse "nil" `shouldBe` mkMalNil

    it "can parse empty lists" $ parse "()" `shouldBe`  mkMalList []

    it "can parse non-empty lists" $ parse "(1 \"Hello\" true false nil some-symbol)"
        `shouldBe`
         mkMalList [ mkMalNumber 1
                   , mkMalString "Hello"
                   , mkMalBool True
                   , mkMalBool False
                   , mkMalNil
                   , mkMalSymbol "some-symbol"
                   ]

    it "can parse nested lists" $ parse "(((1 (\"hello\" true))))"
        `shouldBe`
         mkMalList [ mkMalList [ mkMalList [ mkMalNumber 1
                                           , mkMalList [mkMalString "hello", mkMalBool True]
                                           ]
                               ]
                   ]

    it "can parse maps" $
        parse "{\"hello\" world}" `shouldBe`  mkMalMap [mkMalString "hello", mkMalSymbol "world"]

    it "ignores extra elements when parsing maps" $
        parse "{\"hello\" world 69}" `shouldBe`  mkMalMap [mkMalString "hello", mkMalSymbol "world"]

    it "fails on unterminated string literals" $
        evaluate (parse "\"Hello") `shouldThrow` anyException

main :: IO ()
main = hspec  $ parserSpec >> evaluatorSpec
