module Input.BillSpecificFunding where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

data BillSpecificFunding = BillSpecificFunding
    { bill   :: !Text
    , amount :: !Int
    } deriving (Show, Read, Generic)

instance FromJSON BillSpecificFunding
instance ToJSON BillSpecificFunding
