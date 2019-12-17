module Input.District where

import           Data.Aeson
import           Data.Text                    (Text)
import           GHC.Generics

import           Input.BillSpecificFunding
import           Input.Cap
import           Input.CategoryDefaultFunding

data District = District
    { name                   :: !Text
    , availableFunds         :: !Int
    , categoryDefaultFunding :: ![CategoryDefaultFunding]
    , billSpecificFunding    :: ![BillSpecificFunding]
    , caps                   :: ![Cap]
    } deriving (Show, Read, Generic)

instance FromJSON District
instance ToJSON District
