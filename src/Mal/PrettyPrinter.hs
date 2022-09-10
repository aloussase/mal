{-| Contains a routine to print 'MalType's readably. -}
{-# LANGUAGE TypeApplications #-}
module Mal.PrettyPrinter where

import           Mal.Error
import           Mal.Internal.Util  (unquote)
import           Mal.Types

import qualified Data.Text.IO       as TIO
import           Data.Text.Lazy     (Text, toStrict)
import           Text.Pretty.Simple

ppOptions :: OutputOptions
ppOptions = defaultOutputOptionsDarkBg { outputOptionsStringStyle = Literal }

shOptionsNoColor :: OutputOptions
shOptionsNoColor = defaultOutputOptionsNoColor { outputOptionsStringStyle = Literal }

showReadably :: MalType -> Text
showReadably = pShowOpt shOptionsNoColor

printReadably :: MalType -> IO ()
printReadably = pPrintOpt CheckColorTty ppOptions

errorMsgOptions :: OutputOptions
errorMsgOptions = defaultOutputOptionsDarkBg
    { outputOptionsColorOptions =
        Just defaultColorOptionsDarkBg
            { colorString = colorNull { styleColor = Just (Red, Vivid) }
            }
    }

printError :: MalError -> IO ()
printError err = do
    TIO.putStr . unquote . toStrict . pShowOpt @String errorMsgOptions $ "error: "
    pPrint err
