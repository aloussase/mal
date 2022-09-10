{-| Contains a routine to print 'MalType's readably. -}
{-# LANGUAGE TypeApplications #-}
module Mal.PrettyPrinter where

import           Mal.Error
import           Mal.Internal.Util  (unquote)
import           Mal.Types

import qualified Data.Text.IO       as TIO
import           Data.Text.Lazy     (Text, toStrict)
import           Text.Pretty.Simple

-- | 'generalOutputOptions' are options for general pretty printing (no literal printing).
generalOutputOptions :: OutputOptions
generalOutputOptions = defaultOutputOptionsDarkBg
    { outputOptionsCompactParens = True
    }

-- | 'pPrintOutputOptions' are 'generalOutputOptions' with literal string style.
pPrintOutputOptions :: OutputOptions
pPrintOutputOptions = generalOutputOptions { outputOptionsStringStyle = Literal }

-- | 'showReadably' returns the pretty printed representation of the provided @MalType@
-- as @Text@.
showReadably :: MalType -> Text
showReadably = pShowOpt pPrintOutputOptions

-- | Print the provided 'MalType' as formatted by 'showReadably'.
printReadably :: MalType -> IO ()
printReadably = pPrintOpt CheckColorTty pPrintOutputOptions

-- | Options for printing error messages.
errorMsgOptions :: OutputOptions
errorMsgOptions = generalOutputOptions
    { outputOptionsColorOptions =
        Just defaultColorOptionsDarkBg
            { colorString = colorNull { styleColor = Just (Red, Vivid) }
            }
    }

-- | 'printError' pretty prints error messages.
printError :: MalError -> IO ()
printError err = do
    TIO.putStr . unquote . toStrict . pShowOpt @String errorMsgOptions $ "error: "
    pPrintOpt CheckColorTty generalOutputOptions err
