# safer config

What's wrong with this?

```
data Config =
  Config {
    http_port :: Int
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>  v .:?   "http_port" .!= 8080
  parseJSON _ = fail "Expected Object for Config value"
```

Looks reasonable. Let's try something:


```
main :: IO ()
main = do
  config <- fromJust . eitherDecode =<< getContents
  print (config :: Config)
```

```
httpPort: 9999
```

- Silently defaults to 8080, because `http_port` is not defined.
- obvious here, not necessarily obvious with larger config or multiple stages.

What can we do to make this better?

An attempt that didn't work:
 - flip the script, check each element of the _data_ against the code.
 - Why doesn't this work?
 - A given parser might use multiple keys! No sensible way to check.


We want something that shows we've consumed the whole structure.

to do this, we can turn the thing we got back into a Value, then diff it against what we received!

```
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
```
