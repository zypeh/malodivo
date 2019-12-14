module Main (main) where

import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text
import           Input
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
    where
        parseArgs [inputFileName] = readInputJson inputFileName
        parseArgs []              = putStrLn "No input. Exit now."

readInputJson :: String -> IO ()
readInputJson fileName = do
    content <- Text.readFile fileName
    print content
