{-# LANGUAGE RecordWildCards #-}
module Parliament where

import qualified Data.HashMap.Strict as Map
import           Data.Text           (Text)
import qualified Data.Text as T
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

-- newtype BillMap = BillMap
--     { unBillMap :: Map.HashMap Text (Text, Int)
--     }

-- buildBillMap :: [Bill] -> BillMap
-- buildBillMap bs = BillMap <$> Map.fromList $ (\Bill{..} -> (name, (category, amount))) <$> bs

getDefaultFunding :: District -> [InitialFunding]
getDefaultFunding _d@District{..} =
    (\f@CategoryDefaultFunding{..} -> DefaultFunding name category amount) <$> categoryDefaultFunding

getSpecificFunding :: District -> [InitialFunding]
getSpecificFunding _d@District{..} =
    (\f@BillSpecificFunding{..} -> SpecificFunding dName bill amount) <$> billSpecificFunding
    where dName = name

buildFunding :: [Bill] -> InitialFunding -> [Funding]
buildFunding bills (SpecificFunding districtName billName amount') =
    (\b@Bill{..} -> Funding districtName name category amount') <$> filter (\b@Bill{..} -> name == billName) bills
buildFunding bills (DefaultFunding districtName categoryName amount') =
    (\b@Bill{..} -> Funding districtName name category amount') <$> filter (\b@Bill{..} -> category == categoryName) bills

buildFundingMap :: [Funding] -> Map.HashMap Text Funding
buildFundingMap xs = Map.fromList $ (\f@Funding{..} -> (T.intercalate "-" [districtName, billName], f)) <$> xs