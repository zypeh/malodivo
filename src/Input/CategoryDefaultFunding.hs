module Input.CategoryDefaultFunding where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data CategoryDefaultFunding = CategoryDefaultFunding
    { category :: !Text
    , amount   :: !Int
    } deriving (Show, Read, Generic)

instance FromJSON CategoryDefaultFunding
instance ToJSON CategoryDefaultFunding