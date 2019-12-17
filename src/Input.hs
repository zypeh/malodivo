{-# LANGUAGE DuplicateRecordFields #-}
module Input where

-- import Data.Either (fromRight)
import           Data.Aeson
-- import           Data.Decimal
import           GHC.Generics

import           Input.Bill
import           Input.District

-- No more than 10 bills
-- No more than 50 categories
-- No more than 200 districts
data Input = Input
    { bills     :: ![Bill]
    , districts :: ![District]
    } deriving (Show, Read, Generic)

instance FromJSON Input
instance ToJSON Input
