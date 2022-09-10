{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative     ((<**>), (<|>))
import           Control.Exception       (catch)
import qualified Options.Applicative     as O
import qualified System.Console.Readline as R
import           System.Exit             (exitSuccess)

import           Data.IORef              (IORef)
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
        returnLine line = R.addHistory line >> pure line

eval :: IORef Mal.MalScope -> String -> IO Mal.MalType
eval = Mal.run

print' :: Mal.MalType -> IO ()
print' = Mal.printReadably True

main :: IO ()
main = do
    options      <- O.execParser opts
    initialScope <- Mal.emptyScope

    case input options of
        File _ -> error "not implemented"
        Repl   -> repl initialScope

    where
        repl :: IORef Mal.MalScope -> IO ()
        repl scope = do
            (read' >>= eval scope >>= print') `catch` (\(err :: Mal.MalError) -> Mal.printError err)
            repl scope

        opts = O.info (program <**> O.helper)
                (O.fullDesc
                    <> O.progDesc "The Mal programming language"
                    <> O.header "Mal - A Clojure like programming language")
