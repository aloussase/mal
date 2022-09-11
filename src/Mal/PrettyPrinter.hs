{-| Contains a routine to print 'MalType's readably. -}
module Mal.PrettyPrinter where

import           Mal.Error
import           Mal.Types

import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Data.Text.Lazy     (toStrict)
import           Text.Pretty.Simple

-- | 'showReadably' returns the pretty printed representation of the provided @MalType@
-- as @Text@.
showReadably :: Bool -> MalType -> Text
showReadably True  = unquote
                   . toStrict
                   . pStringOpt defaultOutputOptionsDarkBg
                                    { outputOptionsCompactParens = True
                                    , outputOptionsStringStyle = Literal
                                    }
                   . show

showReadably False = unquote
                   . toStrict
                   . pStringOpt  defaultOutputOptionsNoColor { outputOptionsStringStyle = Literal }
                   . show

-- | Print the provided 'MalType' as formatted by 'showReadably'.
printReadably :: Bool -> MalType -> IO ()
printReadably b t = TIO.putStrLn (showReadably b t)

-- | 'printError' pretty prints error messages.
printError :: MalError -> IO ()
printError err = do
    TIO.putStr
        $ unquote
        $ toStrict
        $ pShowOpt
          defaultOutputOptionsDarkBg
            { outputOptionsColorOptions =
                Just defaultColorOptionsDarkBg
                    { colorString = colorNull { styleColor = Just (Red, Vivid) }
                    }
            }
          ("error: " :: Text)

    pPrintOpt
        CheckColorTty
        defaultOutputOptionsDarkBg { outputOptionsCompactParens = True }
        err

unquote :: Text -> Text
unquote s@"\"" = s
unquote s      = T.filter (/= '\"') s
