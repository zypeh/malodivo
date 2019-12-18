{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Input where

import           Data.Aeson
import           GHC.Generics
import           Data.Text      (Text)

-- No more than 10 bills
-- No more than 50 categories
-- No more than 200 districts

data Bill = Bill
    { name     :: Text
    , category :: Text
    , amount   :: Int
    } deriving (Show, Eq, Generic, FromJSON)

data BillSpecificFunding = BillSpecificFunding
    { bill :: Text
    , amount :: Int
    } deriving (Show, Eq, Generic, FromJSON)

data CategoryDefaultFunding = CategoryDefaultFunding
    { category :: Text
    , amount :: Int
    } deriving (Show, Eq, Generic, FromJSON)

data Cap = Cap
    { category :: Text
    , amount :: Int
    } deriving (Show, Eq, Generic, FromJSON)

data District = District
    { name :: Text
    , availableFunds :: Int
    , categoryDefaultFunding :: [CategoryDefaultFunding]
    , billSpecificFunding :: [BillSpecificFunding]
    , caps :: [Cap]
    } deriving (Show, Generic, FromJSON)

data Input = Input
    { bills     :: [Bill]
    , districts :: [District]
    } deriving (Show, Generic, FromJSON)
