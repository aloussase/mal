module Main (main) where

import           Mal

import           Data.Either (isLeft)
import           Test.Hspec

parserSpec :: Spec
parserSpec = do
    describe "Mal.parse" $ do
        it "can parse atoms" $ do
            parse "1" `shouldBe` Right (mkMalNumber 1)
            parse "\"Hello\"" `shouldBe` Right (mkMalString "Hello")
            parse "true"  `shouldBe` Right (mkMalBool True)
            parse "false"  `shouldBe` Right (mkMalBool False)
            parse "nil" `shouldBe` Right mkMalNil

        it "can parse empty lists" $ do
            parse "()" `shouldBe` Right (mkMalList [])

        it "can parse non-empty lists" $ do
            parse "(1 \"Hello\" true false nil some-symbol)"
                `shouldBe`
                Right (mkMalList [ mkMalNumber 1
                                 , mkMalString "Hello"
                                 , mkMalBool True
                                 , mkMalBool False
                                 , mkMalNil
                                 , mkMalSymbol "some-symbol"
                                 ])

        it "can parse nested lists" $ do
            parse "(((1 (\"hello\" true))))"
                `shouldBe`
                Right (mkMalList [ mkMalList [ mkMalList [ mkMalNumber 1
                                                         , mkMalList [mkMalString "hello", mkMalBool True]
                                                         ]
                                             ]
                                 ])

        it "can parse maps" $ do
            parse "{\"hello\" world}" `shouldBe` Right (mkMalMap [mkMalString "hello", mkMalSymbol "world"])

        it "ignores extra elements when parsing maps" $ do
            parse "{\"hello\" world 69}" `shouldBe` Right (mkMalMap [mkMalString "hello", mkMalSymbol "world"])

        it "fails on unterminated string literals" $ do
            parse "\"Hello" `shouldSatisfy` isLeft

main :: IO ()
main = hspec $ do
    parserSpec
