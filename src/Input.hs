{-# LANGUAGE DuplicateRecordFields #-}
module Input where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

-- No more than 10 bills
-- No more than 50 categories
-- No more than 200 districts

data Bill = Bill
    { name     :: !Text
    , category :: !Text
    , amount   :: !Int
    } deriving (Show, Read, Generic)

data CategoryDefaultFunding = CategoryDefaultFunding
    { category :: !Text
    , amount   :: !Int
    } deriving (Show, Read, Generic)

data BillSpecificFunding = BillSpecificFunding
    { bill   :: !Text
    , amount :: !Int
    } deriving (Show, Read, Generic)

data Cap = Cap
    { category :: !Text
    , amount   :: !Int
    } deriving (Show, Read, Generic)

data District = District
    { name                        :: !Text
    , availableFunds              :: !Int
    , categoryDefaultFunding      :: ![CategoryDefaultFunding]
    , districtBillSpecificFunding :: ![BillSpecificFunding]
    , caps                        :: ![Cap]
    } deriving (Show, Read, Generic)

data Input = Input
    { bills     :: ![Bill]
    , districts :: ![District]
    } deriving (Show, Read, Generic)

instance FromJSON Bill
instance FromJSON CategoryDefaultFunding
instance FromJSON BillSpecificFunding
instance FromJSON Cap
instance FromJSON District
instance FromJSON Input

instance ToJSON Bill
instance ToJSON CategoryDefaultFunding
instance ToJSON BillSpecificFunding
instance ToJSON Cap
instance ToJSON District
instance ToJSON Input
