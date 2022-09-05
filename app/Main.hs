module Main where

import           Control.Applicative     ((<**>), (<|>))
import qualified Options.Applicative     as O
import qualified System.Console.Readline as R
import           System.Exit             (exitSuccess)

import qualified Mal

data Input = File String | Repl
newtype ProgramOptions = MkProgramOptions { input :: Input }

fileInput :: O.Parser Input
fileInput = File <$> O.strArgument (O.metavar "filename")

replInput :: O.Parser Input
replInput = pure Repl

program :: O.Parser ProgramOptions
program = MkProgramOptions <$> (fileInput <|> replInput)

read' :: IO String
read' = R.readline "$ " >>= maybe (putStrLn "bye bye!" >> exitSuccess) returnLine
    where
        returnLine line = do
            R.addHistory line
            pure line

eval :: String -> Mal.MalType
eval = Mal.parse

print' :: Mal.MalType -> IO ()
print' = print

main :: IO ()
main = do
    options <- O.execParser opts
    case input options of
        File _ -> error "not implemented"
        Repl   -> repl

    where
        repl = read' >>= print' . eval >> repl

        opts = O.info (program <**> O.helper)
                (O.fullDesc
                    <> O.progDesc "The Mal programming language"
                    <> O.header "Mal - A Clojure like programming language")
