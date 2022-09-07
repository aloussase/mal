module Mal.Internal.Parser (parse) where

import           Mal.Error
import           Mal.Internal.Types

import           Prelude                    hiding (readList)

import           Control.Monad              (void)
import           Data.Functor               ((<&>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            hiding (parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

spaceOrEof :: Parser ()
spaceOrEof = void spaceChar <|> eof

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

readNil :: Parser MalType
readNil = symbol "nil" >> pure (MalAtom MalNil)

readNumber:: Parser MalType
readNumber = MalAtom . MalNumber <$> lexeme L.decimal

readString :: Parser MalType
readString = MalAtom . MalString <$> lexeme parseString
    where
        parseString = symbol "\"" >> manyTill L.charLiteral (symbol "\"")

readSymbol :: Parser MalType
readSymbol = do
    fst <- letterChar
    rest <- lexeme (manyTill L.charLiteral spaceOrEof)
    pure $ MalAtom (MalSymbol $ fst:rest)

readBool :: Parser MalType
readBool = MalAtom <$> (parseTrue <|> parseFalse)
    where
        parseTrue = symbol "true" >> pure (MalBool True)
        parseFalse = symbol "false" >> pure (MalBool False)

readList :: Parser MalType
readList = symbol "(" *> lexeme (many readForm) <* symbol ")" <&> MalList

readAtom :: Parser MalType
readAtom = choice
    [ try readBool
    , try readNil
    , readString
    , readSymbol
    , readNumber
    ]

readForm :: Parser MalType
readForm = label "valid mal expression" $ readList <|> readAtom

parse :: String -> Either MalError MalType
parse input = do
    case runParser (space >> readForm <* eof) "<repl>" (T.pack input) of
        Left err     -> Left $ ParseError $ errorBundlePretty err
        Right result -> Right result
