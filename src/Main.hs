module Main (main) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as Map
import           Input
import           Parliament
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
        Right ps -> do
            ------ STEP 1 ------
            let defaultFunding = getDefaultFunding <$> (districts ps)
            let specificFunding = getSpecificFunding <$> (districts ps)

            let defaults = concat $ buildFunding (bills ps) <$> (concat defaultFunding)
            let specifics = concat $ buildFunding (bills ps) <$> (concat specificFunding)

            let defaultMap = buildFundingMap defaults
            let specificMap = buildFundingMap specifics

            let fundings = snd <$> (Map.toList $ Map.union specificMap defaultMap)

            -------- STEP 2 -----------
            print $ findDistrictRatio fundings <$> districts ps
            -- print $ updateFunding 0 <$> fundings
            -- print . totalFundedPerCategory $ snd <$> fundings
