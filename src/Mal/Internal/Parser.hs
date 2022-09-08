module Mal.Internal.Parser (parse) where

import           Mal.Error
import           Mal.Internal.Types
import           Mal.Internal.Util          (pairs)

import           Prelude                    hiding (readList)

import           Control.Exception          (throw)
import           Control.Monad              (void)
import           Data.Functor               ((<&>))
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
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
readNil = label "nil" (symbol "nil") >> pure mkMalNil

readNumber:: Parser MalType
readNumber = mkMalNumber <$> L.decimal <?> "number"

readString :: Parser MalType
readString = mkMalString <$> label "string" parseString
    where
        parseString = symbol "\"" >> L.charLiteral `manyTill` symbol "\""

readSymbol :: Parser MalType
readSymbol = label "symbol" $
    mkMalSymbol <$> some (choice
        [alphaNumChar,  char '-', char '+', char '/', char '*'])

readBool :: Parser MalType
readBool = label "bool" (parseTrue <|> parseFalse)
    where
        parseTrue = symbol "true" >> pure (mkMalBool True)
        parseFalse = symbol "false" >> pure (mkMalBool False)

readListLike :: (MalListLike a) => String -> Text -> Text -> ([MalType] -> a) -> Parser a
readListLike lbl start end f =  label lbl (symbol start *> many readForm <* symbol end <&> f)

readList :: Parser MalType
readList = MalList <$> readListLike "list" "(" ")" MkMalList

readVector :: Parser MalType
readVector = MalVec <$> readListLike "vector" "[" "]" (MkMalVec . V.fromList)

readMap :: Parser MalType
readMap = MalMap <$> readListLike "hash-map" "{" "}" (MkMalMap . M.fromList . pairs)

readAtom :: Parser MalType
readAtom = label "atom" $ choice
    [ try readBool
    , try readNil
    , readString
    , try readNumber
    , try readSymbol
    ]

readComment :: Parser MalType
readComment = label "comment" $ do
    -- TODO: throw an error to signal a comment.
    symbol ";"
    anySingle `manyTill` (void newline <|> eof)
    pure mkMalNil

readForm :: Parser MalType
readForm = label "valid mal expression" $ lexeme
    (choice [readComment, readList, readAtom, readVector, readMap])

parse :: String -> MalType
parse input =
    case runParser (space >> readForm <* eof) "<repl>" (T.pack input) of
        Left err     -> throw $ ParseError (errorBundlePretty err)
        Right result -> result
