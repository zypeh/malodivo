{-# LANGUAGE RecordWildCards #-}
module Parliament where

import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Input

data InitialFunding
    = DefaultFunding Text Text Int -- DefaultFunding DistrictName CategoryName Amount
    | SpecificFunding Text Text Int -- SpecificFunding DistrictName BillName Amount
    deriving (Show)

data Funding = Funding
    { districtName :: Text
    , billName     :: Text
    , billCategory :: Text
    , billAmount   :: Int
    } deriving (Show)

updateFunding :: Int -> Funding-> Funding
updateFunding amount x@Funding{..} = x { billAmount = amount }

getDefaultFunding :: District -> [InitialFunding]
getDefaultFunding District{..} =
    (\CategoryDefaultFunding{..} -> DefaultFunding name category amount) <$> categoryDefaultFunding

getSpecificFunding :: District -> [InitialFunding]
getSpecificFunding District{..} =
    (\BillSpecificFunding{..} -> SpecificFunding dName bill amount) <$> billSpecificFunding
    where dName = name

getCategoryCap :: District -> [(Text, Int)]
getCategoryCap District{..} = (\Cap{..} -> (category, amount)) <$> caps

buildFunding :: [Bill] -> InitialFunding -> [Funding]
buildFunding bills (SpecificFunding districtName billName amount') =
    (\Bill{..} -> Funding districtName name category amount') <$> filter (\Bill{..} -> name == billName) bills
buildFunding bills (DefaultFunding districtName categoryName amount') =
    (\Bill{..} -> Funding districtName name category amount') <$> filter (\Bill{..} -> category == categoryName) bills

buildFundingMap :: [Funding] -> Map.HashMap Text Funding
buildFundingMap xs = Map.fromList $ (\f@Funding{..} -> (T.intercalate "-" [districtName, billName], f)) <$> xs

totalFundedPerCategory :: [Funding] -> [(Text, Int)]
totalFundedPerCategory fundings =
    Map.toList $ foldr merge Map.empty ffundings
    where
        ffundings = fmap (\Funding{..} -> (billCategory, billAmount)) fundings
        merge (category, amount) m = Map.insertWith (+) category amount m

totalFundedPerDistrict :: [Funding] -> [(Text, Int)]
totalFundedPerDistrict fundings =
    Map.toList $ foldr merge Map.empty ffundings
    where
        ffundings = fmap (\Funding{..} -> (districtName, billAmount)) fundings
        merge (dName, amount) m = Map.insertWith (+) dName amount m

fundCapRatio :: (Int, Int) -> Double -- Double precision is enough
fundCapRatio (fund, cap) = (fromIntegral cap) / (fromIntegral fund)

findDistrictRatio :: [Funding] -> District -> [Funding]
findDistrictRatio fundings d@District{..} = 
    adjustFundingBasedOnRatio ratio <$> fundingsInThisDistrict
    where
        ratio = minimum $ fundCapRatio <$>
            buildRatioList (getCategoryCap d) (totalFundedPerCategory fundingsInThisDistrict)

        fundingsInThisDistrict = filter (\Funding{..} -> districtName == name) fundings

        buildRatioList :: [(Text, Int)] -> [(Text, Int)] -> [(Int, Int)]
        buildRatioList ((categoryName, cap):xs) l = (fromJust (lookup categoryName l), cap) : buildRatioList xs l
        buildRatioList [] _ = []

adjustFundingBasedOnRatio :: Double -> Funding -> Funding
adjustFundingBasedOnRatio ratio f@Funding{..} = updateFunding (round $ ratio * fromIntegral billAmount) f
