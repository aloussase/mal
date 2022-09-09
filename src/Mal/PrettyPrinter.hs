{-| Contains a routine to print 'MalType's readably. -}
module Mal.PrettyPrinter where

import           Mal.Error
import           Mal.Internal.Util  (unquote)
import           Mal.Types

import qualified Data.Text.IO       as TIO
import           Data.Text.Lazy     (Text, toStrict, unpack)
import           Text.Pretty.Simple

ppOptions = defaultOutputOptionsDarkBg { outputOptionsStringStyle = Literal }
shOptionsNoColor = defaultOutputOptionsNoColor { outputOptionsStringStyle = Literal }

showReadably :: MalType -> Text
showReadably = pShowOpt shOptionsNoColor

printReadably :: MalType -> IO ()
printReadably = pPrintOpt CheckColorTty ppOptions

errorMsgOptions = defaultOutputOptionsDarkBg
    { outputOptionsColorOptions =
        Just defaultColorOptionsDarkBg
            { colorString = colorNull { styleColor = Just (Red, Vivid) }
            }
    }

printError :: MalError -> IO ()
printError err = do
    TIO.putStr . unquote . toStrict . pShowOpt errorMsgOptions $ "error: "
    pPrint err
