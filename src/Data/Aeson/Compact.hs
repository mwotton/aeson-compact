module Data.Aeson.Compact(CompactParser(..)) where

import           Data.Aeson       (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Diff  as D
import           Data.Aeson.Types (Parser)

newtype CompactParser a = CompactParser { unCompactParser :: a }

instance (ToJSON a, FromJSON a) => FromJSON (CompactParser a) where
  parseJSON v = do
    result <- parseJSON v
    case D.diff v (toJSON result) of
      D.Patch [] -> pure $ CompactParser result
      xs         -> fail ("parser is not compact!" <> show xs)
