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
sc = L.space space1 (L.skipLineComment ";") (L.skipLineComment ";")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

readNil :: Parser MalType
readNil = label "nil" (symbol "nil") >> pure (MalAtom MalNil)

readNumber:: Parser MalType
readNumber = MalAtom . MalNumber <$> L.decimal <?> "number"

readString :: Parser MalType
readString = MalAtom . MalString <$> label "string" parseString
    where
        parseString = do
            symbol "\""
            L.charLiteral `manyTill` symbol "\""

readSymbol :: Parser MalType
readSymbol = label "symbol" $ do
    fst <- letterChar
    rest <- many $ choice [alphaNumChar,  char '-']
    pure $ MalAtom (MalSymbol $ fst:rest)

readBool :: Parser MalType
readBool = MalAtom <$> label "bool" (parseTrue <|> parseFalse)
    where
        parseTrue = symbol "true" >> pure (MalBool True)
        parseFalse = symbol "false" >> pure (MalBool False)

readList :: Parser MalType
readList = label "list" (symbol "(" *> many readForm <* symbol ")" <&> MalList)

readAtom :: Parser MalType
readAtom = label "atom" $ choice
    [ try readBool
    , try readNil
    , readString
    , readSymbol
    , readNumber
    ]

readComment :: Parser MalType
readComment = label "comment" $ do
    -- TODO: throw an error to signal a comment.
    symbol ";"
    anySingle `manyTill` (void newline <|> eof)
    pure $ MalAtom MalNil

readForm :: Parser MalType
readForm = label "valid mal expression" $ lexeme (readComment <|> readList <|> readAtom)

parse :: String -> Either MalError MalType
parse input = do
    case runParser (space >> readForm <* eof) "<repl>" (T.pack input) of
        Left err     -> Left $ ParseError $ errorBundlePretty err
        Right result -> Right result
