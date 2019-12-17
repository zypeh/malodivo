module Output where

import           Data.Aeson
import           Data.Text                    (Text)
import           GHC.Generics

import           Input.Cap

data CategoryFunding = CategoryFunding
    { category :: !Text
    , amount   :: !Int
    } deriving (Show, Read, Generic)

data District = District
    { name            :: !Text
    , availableFunds  :: !Int
    , remainingFunds   :: !Int
    , categoryFunding :: ![CategoryFunding]
    , caps            :: ![Cap]
    } deriving (Show, Read, Generic)

instance ToJSON District
instance FromJSON CategoryFunding
instance ToJSON CategoryFunding