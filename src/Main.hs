module Main (main) where

import           Parliament
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Input
import           System.Environment   (getArgs)
import Data.Ord
import Data.List
import qualified Data.HashMap.Strict as Map

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
        Right ps -> do
            let defaultFunding = getDefaultFunding <$> (districts ps)
            let specificFunding = getSpecificFunding <$> (districts ps)

            let defaults = concat $ buildFunding (bills ps) <$> (concat defaultFunding)
            let specifics = concat $ buildFunding (bills ps) <$> (concat specificFunding)

            let defaultMap = buildFundingMap defaults
            let specificMap = buildFundingMap specifics

            -- print . show $ defaults
            print . Map.toList $ Map.union specificMap defaultMap
            -- print . show $ defaultMap
            -- print $ sortBy (comparing ) $ concat $ buildFunding (bills ps) <$> initialFunds
            -- print "x"