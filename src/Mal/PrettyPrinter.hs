{-| Contains a routine to print 'MalType's readably. -}
module Mal.PrettyPrinter where

import           Mal.Error
import           Mal.Types

import qualified Data.Text          as T
import           Data.Text.Lazy     (Text)
import qualified Data.Text.Lazy.IO  as TIO
import           Text.Pretty.Simple

-- | 'showReadably' returns the pretty printed representation of the provided @MalType@
-- as @Text@.
showReadably :: Bool -> MalType -> Text
showReadably True  = pStringOpt defaultOutputOptionsDarkBg
                                    { outputOptionsCompactParens = True
                                    , outputOptionsStringStyle = Literal
                                    }
                   . unquoteString
                   . unescapeString
                   . show
showReadably False = pStringOpt  defaultOutputOptionsNoColor { outputOptionsStringStyle = Literal }
                   . unquoteString
                   . unescapeString
                   . show

-- | Print the provided 'MalType' as formatted by 'showReadably'.
printReadably :: Bool -> MalType -> IO ()
printReadably b t = TIO.putStrLn (showReadably b t)

printStringReadably :: String -> IO ()
printStringReadably = print
                    . unquoteString
                    . unescapeString
                    . show

-- | 'printError' pretty prints error messages.
printError :: MalError -> IO ()
printError err = do
    pPrintStringOpt CheckColorTty
        defaultOutputOptionsDarkBg
            { outputOptionsCompactParens = True
            , outputOptionsStringStyle = Literal
            , outputOptionsColorOptions =
                Just defaultColorOptionsDarkBg
                    { colorString = colorNull { styleColor = Just (Red, Vivid) }
                    }
            } "error: "

    pPrintOpt
        CheckColorTty
        defaultOutputOptionsDarkBg
            { outputOptionsCompactParens = True
            , outputOptionsStringStyle = Literal
            }
        err

-- | Unescape a @String@, replacing escape characters by the '\' + c character.
unescapeString :: String -> String
unescapeString = go []
    where
        go :: String -> String -> String
        go acc ('\\':'n':xs) = go (acc ++ "\\n") xs
        go acc (x:xs)        = go (acc ++ [x]) xs
        go acc []            = acc

-- | 'unquouteString' removes all '"' characters from a string, unless the string
-- is "\"".
unquote :: T.Text -> T.Text
unquote s@"\"" = s
unquote s      = T.filter (/= '\"') s

-- | Same as 'unquoteString' buy for @String@s instead of @Text@.
unquoteString :: String -> String
unquoteString = T.unpack . unquote . T.pack
