module Main (main) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Input
import           System.Environment   (getArgs)

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
    where
        parseArgs [inputFileName] = readInputJson inputFileName
        parseArgs _               = putStrLn "No input. Exit now."

readInputJson :: String -> IO ()
readInputJson fileName = do
    content <- eitherDecode <$> BL.readFile fileName :: IO (Either String Input)
    case content of
        Left err -> putStrLn err
        Right ps -> print ps
