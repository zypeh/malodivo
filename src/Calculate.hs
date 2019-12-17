module Calculate where

import           Data.Ratio
import           Data.Text  (Text)
import qualified Input
import qualified Output

-- Find the fund cap distance
-- !! Make sure the cap is not smaller than the fund given
fundCapRatio:: (Int, Int) -> Ratio Int
fundCapRatio (fund, cap) = cap % fund

-- Take the smallest ratio
smallestFundCapRatio :: [(Int, Int)] -> Ratio Int
smallestFundCapRatio xs = minimum (fundCapRatio <$> xs)

findDistrictFund :: Input.District -> Output.District
findDistrictFund district = do
    let defaultFundings = Input.categoryDefaultFunding district
    let caps = Input.caps district
    let ratio = smallestFundCapRatio $ buildRatioList defaultFundings caps
    Output.District (Input.name district) (Input.availableFunds district) (Input.availableFunds district) [] []
    where
        buildRatioList :: [Input.CategoryDefaultFunding] -> [Input.Cap] -> [(Int, Int)]
        buildRatioList (f:fs) caps' = (Input.amount f, Input.amount $ findCapByCategory caps' . Input.name f) : buildRatioList fs caps'
        buildRatioList [] caps = [(1, 1)]

findCapByCategory :: [Input.Cap] -> Text -> Input.Cap
findCapByCategory caps categoryName = head $ filter (\cap -> category cap == categoryName) caps
