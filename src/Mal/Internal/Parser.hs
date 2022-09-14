{-| Parses a Mal program into an AST suitable for evaluation. -}
module Mal.Internal.Parser (parse) where

import           Mal.Error
import           Mal.Internal.Util          (pairs)
import           Mal.Types

import           Prelude                    hiding (readList)

import           Control.Exception          (throw)
import           Control.Monad              (void)
import           Data.Functor               ((<&>))
import qualified Data.Map                   as M
import           Data.Text                  (Text)
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

readNumber:: Parser MalType
readNumber = mkMalNumber <$> L.decimal <?> "number"

readString :: Parser MalType
readString = mkMalString <$> label "string" parseString
    where
        parseString = symbol "\"" >> L.charLiteral `manyTill` symbol "\""

readSymbol :: Parser MalType
readSymbol = label "symbol" $ do
    token' <- some (choice
        [ alphaNumChar
        , char '-', char '+', char '/', char '*', char '!', char '?'
        , char '>', char '=', char '&', char '<', char '='
        ])
    pure $ case token' of
        "nil"   -> mkMalNil
        "true"  -> mkMalBool True
        "false" -> mkMalBool False
        _       -> mkMalSymbol token'

readListLike :: (MalListLike a) => String -> Text -> Text -> ([MalType] -> a) -> Parser a
readListLike lbl start end f =  label lbl (symbol start *> many readForm <* symbol end <&> f)

readList :: Parser MalType
readList = MalList <$> readListLike "list" "(" ")" MkMalList

readVector :: Parser MalType
readVector = MalVec <$> readListLike "vector" "[" "]" (MkMalVector . V.fromList)

readMap :: Parser MalType
readMap = MalMap <$> readListLike "hash-map" "{" "}" (MkMalMap . M.fromList . pairs)

readAtom :: Parser MalType
readAtom = label "atom" $ choice
    [ readNumber
    , readSymbol
    , readString
    ]

readComment :: Parser MalType
readComment = label "comment" $
    symbol ";" >> anySingle `manyTill` (void newline <|> eof) >> pure mkMalNil

readQuote :: Parser MalType
readQuote = do
    _ <- char '\''
    form <- readForm
    pure $ mkMalList ["quote", form]

readQuasiquote :: Parser MalType
readQuasiquote = do
    _ <- char '`'
    form <- readForm
    pure $ mkMalList ["quasiquote", form]

readUnquote :: Parser MalType
readUnquote = do
    _ <- char '~'
    form <- readForm
    pure $ mkMalList ["unquote", form]

readSpliceUnquote :: Parser MalType
readSpliceUnquote = do
    _ <- string "~@"
    form <- readForm
    pure $ mkMalList ["splice-unquote", form]

readerMacro :: Parser MalType
readerMacro = readQuote <|> readQuasiquote <|> try readSpliceUnquote <|> readUnquote

readForm :: Parser MalType
readForm = label "valid mal expression" $ lexeme
    (choice [readComment, readList, readAtom, readVector, readMap, readerMacro])

-- | 'parse' parses the provided Mal program as a @String@ and returns the resulting AST.
parse :: Maybe MalFilename -> Text -> MalType
parse filename input =
    let filename' = case filename of
                        Just (MkMalFilename f) -> f
                        _                      -> "<repl>"
     in
        case runParser (space >> readForm <* eof) filename' input of
          Left err     -> throw $ ParseError (errorBundlePretty err)
          Right result -> result
