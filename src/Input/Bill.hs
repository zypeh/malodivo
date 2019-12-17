module Input.Bill where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

data Bill = Bill
    { name     :: !Text
    , category :: !Text
    , amount   :: !Int
    } deriving (Show, Read, Generic)

instance FromJSON Bill
instance ToJSON Bill