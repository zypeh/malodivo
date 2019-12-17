module Calculate where

import           Data.Text                    (Text)
import qualified Input.Cap
import qualified Input.CategoryDefaultFunding
import qualified Input.District
import qualified Output

-- Find the fund cap distance
fundCapRatio:: (Int, Int) -> Double
fundCapRatio (fund, cap) = (fromIntegral cap) / (fromIntegral fund)

-- Take the smallest ratio
smallestFundCapRatio :: [(Int, Int)] -> Double
smallestFundCapRatio xs = minimum $ fundCapRatio <$> xs

updateCategoryFunding :: Double -> Input.CategoryDefaultFunding.CategoryDefaultFunding -> Output.CategoryFunding
updateCategoryFunding ratio funding =
    Output.CategoryFunding
        (Input.CategoryDefaultFunding.category funding)
        (round $ ratio * (fromIntegral $ Input.CategoryDefaultFunding.amount funding))

findDistrictFund :: Input.District.District -> Output.District
findDistrictFund district = do
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
