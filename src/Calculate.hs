{-# LANGUAGE RecordWildCards #-} -- Not sure how this works
module Calculate where

import           Data.Function                (on)
import qualified Data.HashMap.Strict          as Map
import           Data.List
import           Data.Ord                     (comparing)
import           Data.Text                    (Text)
import qualified Input.Bill
import qualified Input.Cap
import qualified Input.CategoryDefaultFunding
import qualified Input.District
import qualified Output

-- Find the fund cap distance
fundCapRatio :: (Int, Int) -> Double
fundCapRatio (fund, cap) = (fromIntegral cap) / (fromIntegral fund)

-- Take the smallest ratio
smallestFundCapRatio :: [(Int, Int)] -> Double
smallestFundCapRatio xs = minimum $ fundCapRatio <$> xs

-- Find the margin between the funds needed and the fund allocated
fundLimitRatio :: (Int, Int) -> Double
fundLimitRatio = fundCapRatio

updateCategoryFunding :: Double -> Input.CategoryDefaultFunding.CategoryDefaultFunding -> Output.CategoryFunding
updateCategoryFunding ratio funding =
    Output.CategoryFunding
        (Input.CategoryDefaultFunding.category funding)
        (round $ ratio * (fromIntegral $ Input.CategoryDefaultFunding.amount funding))

calculateDistrictFund :: Input.District.District -> Output.District
calculateDistrictFund district = do
    let defaultFundings = Input.District.categoryDefaultFunding district
    let caps = Input.District.caps district
    let ratio = smallestFundCapRatio $ buildRatioList defaultFundings caps
    let fundings = updateCategoryFunding ratio <$> defaultFundings
    let totalFunded = sum $ fmap (\f -> Output.amount f) fundings
    Output.District
        (Input.District.name district)
        (Input.District.availableFunds district)
        ((Input.District.availableFunds district) - totalFunded)
        fundings
        (Input.District.caps district)
    where
        buildRatioList :: [Input.CategoryDefaultFunding.CategoryDefaultFunding] -> [Input.Cap.Cap] -> [(Int, Int)]
        buildRatioList (f:fs) caps' =
            (Input.CategoryDefaultFunding.amount f, Input.Cap.amount $ findCapByCategory caps' $ Input.CategoryDefaultFunding.category f) : buildRatioList fs caps'
        buildRatioList [] _ = []

findCapByCategory :: [Input.Cap.Cap] -> Text -> Input.Cap.Cap
findCapByCategory caps categoryName = head $ filter (\cap -> Input.Cap.category cap == categoryName) caps

-- findCategoryByBillName :: [Input.Bill.Bill] -> Text -> Input.Bill.Bill

totalFundsNeededPerCategory :: [Input.Bill.Bill] -> [(Text, Int)]
totalFundsNeededPerCategory bills =
    merge . groupBy ((==) `on` fst) . sortBy (comparing fst) $ fmap (\b -> (Input.Bill.category b, Input.Bill.amount b)) bills
    where
        merge = fmap (\l -> (fst . head $ l, sum $ snd <$> l))

-- returns [(CategoryName, TotalFunded, NumberOfFunding)]
totalFundsFundedPerCategory :: [Output.District] -> [(Text, Int, Int)]
totalFundsFundedPerCategory districts =
    merge . groupBy ((==) `on` fst) . sortBy (comparing fst) $ fmap (\f -> (Output.category f, Output.amount f)) $ Output.categoryFunding =<< districts
    where
        merge = fmap (\l -> (fst . head $ l, sum $ snd <$> l, length $ snd <$> l))

adjustDistrictFund :: [Output.District] -> [(Text, Int)] -> [Output.District]
adjustDistrictFund districts categoryFundingLimit = do
    let fundingFundedPerDistrict = totalFundsFundedPerCategory districts
    let categoryFundMap = Map.fromList categoryFundingLimit
    let adjustments = checkExceedCategoryLimit fundingFundedPerDistrict categoryFundMap
    adjust districts adjustments
    where
        checkExceedCategoryLimit :: [(Text, Int, Int)] -> Map.HashMap Text Int -> [(Text, Double)]
        checkExceedCategoryLimit [] _ = []
        checkExceedCategoryLimit (funding:fs) categoryFundingLimitMap = do
            let (categoryName, funded, numOfFund) = funding
            let fundLimit = categoryFundingLimitMap Map.! categoryName
            case funded > fundLimit of
                True -> (categoryName, (fundLimitRatio (funded, fundLimit)) / (fromIntegral numOfFund)) : checkExceedCategoryLimit fs categoryFundingLimitMap
                False -> checkExceedCategoryLimit fs categoryFundingLimitMap

        adjust :: [Output.District] -> [(Text, Double)] -> [Output.District]
        adjust districts adjustment = districts