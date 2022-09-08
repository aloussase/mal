module Main (main) where

import           Mal

import           Control.Exception (evaluate)
import           Test.Hspec

evaluatorSpec :: Spec
evaluatorSpec = describe "Mal.run" $ do
    context "arithmetic" $ do
        it "can evaluate simple arithmetic" $ do
            run "(+ 1 2)" `shouldReturn` mkMalNumber 3
            run "(* 1 2)" `shouldReturn` mkMalNumber 2
            run "(/ 2 1)" `shouldReturn` mkMalNumber 2
            run "(- 2 1)" `shouldReturn` mkMalNumber (-3)

        it "can evalute expressions nested in vectors" $ do
            run "[(+ 1 2) (* 2 3 (+ 3 2))]" `shouldReturn` mkMalVector [mkMalNumber 3, mkMalNumber 30]

        it "can evaluate expression nested in hash maps" $ do
            run "{hello (* 3 2) world (+ 5 (* 2 1))}"
                `shouldReturn` mkMalMap [mkMalSymbol "hello", mkMalNumber 6, mkMalSymbol "world", mkMalNumber 7]

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
