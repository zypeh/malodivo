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
            let updated_fundings = concat $ findDistrictRatio fundings <$> districts ps

            -------- STEP 3 -----------
            let checkedDistrictAvailableFunds = concat $
                    checkAvailableFunds (districts ps) updated_fundings <$>
                    totalFundedPerDistrict updated_fundings

            -------- STEP 4 -----------
            let ratio = minimum $ checkFundsNeededPerBill (bills ps) <$>
                    totalFundedPerBill checkedDistrictAvailableFunds

            let finalFundings = adjustFundingBasedOnRatio ratio <$> checkedDistrictAvailableFunds
            print finalFundings