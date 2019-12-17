module Input.Cap where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

data Cap = Cap
    { category :: !Text
    , amount   :: !Int
    } deriving (Show, Read, Generic)

instance FromJSON Cap
instance ToJSON Cap
