module Main (main) where

import           Mal

import           Test.Hspec

parserSpec :: Spec
parserSpec = do
    describe "Mal.parse" $ do
        it "can parse atoms" $ do
            parse "1" `shouldBe` MalAtom (MalNumber 1)
            parse "\"Hello\"" `shouldBe` MalAtom (MalString "Hello")
            parse "true"  `shouldBe` MalAtom (MalBool True)
            parse "false"  `shouldBe` MalAtom (MalBool False)
            parse "nil" `shouldBe` MalAtom MalNil

        it "can parse empty lists" $ do
            parse "()" `shouldBe` MalList []

        it "can parse non-empty lists" $ do
            parse "(1 \"Hello\" true false nil)"
                `shouldBe`
                MalList [ MalAtom (MalNumber 1)
                        , MalAtom (MalString "Hello")
                        , MalAtom (MalBool True)
                        , MalAtom (MalBool False)
                        , MalAtom MalNil
                        ]

        it "can parse nested lists" $ do
            parse "(((1 (\"hello\" true))))"
                `shouldBe`
                MalList [ MalList [ MalList [ MalAtom (MalNumber 1)
                                            , MalList [MalAtom (MalString "hello"), MalAtom (MalBool True)]
                                            ]
                                  ]
                        ]

main :: IO ()
main = hspec $ do
    parserSpec
