module Main (main) where

import           Mal

import           Data.Either (isLeft)
import           Test.Hspec

parserSpec :: Spec
parserSpec = do
    describe "Mal.parse" $ do
        it "can parse atoms" $ do
            parse "1" `shouldBe` Right (MalAtom (MalNumber 1))
            parse "\"Hello\"" `shouldBe` Right (MalAtom (MalString "Hello"))
            parse "true"  `shouldBe` Right (MalAtom (MalBool True))
            parse "false"  `shouldBe` Right (MalAtom (MalBool False))
            parse "nil" `shouldBe` Right (MalAtom MalNil)

        it "can parse empty lists" $ do
            parse "()" `shouldBe` Right (MalList [])

        it "can parse non-empty lists" $ do
            parse "(1 \"Hello\" true false nil some-symbol)"
                `shouldBe`
                Right (MalList [ MalAtom (MalNumber 1)
                               , MalAtom (MalString "Hello")
                               , MalAtom (MalBool True)
                               , MalAtom (MalBool False)
                               , MalAtom MalNil
                               , MalAtom (MalSymbol "some-symbol")
                               ])

        it "can parse nested lists" $ do
            parse "(((1 (\"hello\" true))))"
                `shouldBe`
                Right (MalList [ MalList [ MalList [ MalAtom (MalNumber 1)
                                                     , MalList [MalAtom (MalString "hello"), MalAtom (MalBool True)]
                                                   ]
                                         ]
                               ])

        it "fails on unterminated string literals" $ do
            parse "\"Hello" `shouldSatisfy` isLeft

main :: IO ()
main = hspec $ do
    parserSpec
