{-# LANGUAGE RecordWildCards #-}
module PrettyPrinter where

import           Data.Function (on)
import           Data.List
import           Data.Ord      (comparing)
import           Data.Text     (Text)
import qualified Data.Text     as Text
import           Data.Tree

import           Input
import           Parliament

groupByDistrict :: [Funding] -> [(Text, [Funding])]
groupByDistrict fundings = concat' . groupBy ((==) `on` fst) . sortBy (comparing fst) $
    (\f@Funding{..} -> (districtName, f)) <$> fundings
    where
        concat' = fmap (\l -> (fst . head $ l, snd <$> l))

asTree :: [District] -> (Text, [Funding]) -> Tree String
asTree districts (dName, fundings) =
        Node
            ("District: \t\t" ++ Text.unpack dName ++ "\n"
            ++ "Available Funds: \t$" ++ show districtFundLimit ++ "\n"
            ++ "Total Funds: \t\t$" ++ show totalUsed ++ "\n"
            ++ "Leftover Amount: \t$" ++ show (districtFundLimit - totalUsed))
            (mapBill fundings)
    where
        districtFundLimit = availableFunds . head $ filter (\District{..} -> dName == name) districts
        totalUsed = sum $ billAmount <$> fundings

        mapBill [] = []
        mapBill (Funding{..}:fs) = Node (
            "Bill: " ++ Text.unpack billName ++ "\n"
            ++ "Category: " ++ Text.unpack billCategory ++ "\n"
            ++ "Funded: $" ++ show billAmount) []
            : mapBill fs

prettyPrint :: [District] -> [Funding] -> [String]
prettyPrint districts x = drawTree <$> asTree districts <$> groupByDistrict x
