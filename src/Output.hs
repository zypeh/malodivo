{-# LANGUAGE DuplicateRecordFields #-}
module Output where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

data CategoryFunding = CategoryFunding
    { category :: !Text
    , amount   :: !Int
    } deriving (Show, Read, Generic)

data Cap = Cap
    { category :: !Text
    , amount   :: !Int
    } deriving (Show, Read, Generic)

data District = District
    { name            :: !Text
    , availableFunds  :: !Int
    , remaningFunds   :: !Int
    , categoryFunding :: ![CategoryFunding]
    , caps            :: ![Cap]
    } deriving (Show, Read, Generic)

instance ToJSON CategoryFunding
instance ToJSON Cap
instance ToJSON District