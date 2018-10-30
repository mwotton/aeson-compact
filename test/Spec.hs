{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Data.Aeson
import           Data.Aeson.Compact
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

data Config =
  Config {
    http_port :: Int
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>  v .:?   "http_port" .!= 8080
  parseJSON _ = fail "Expected Object for Config value"

instance ToJSON Config where
  toJSON (Config{..}) = object ["http_port" .= http_port]

instance Arbitrary Config where
  arbitrary = Config <$> arbitrary

spec :: Spec
spec = describe "aeson-compact" $ do
  -- just to check our aeson stuff is reasonable
  it "is round-trippable" $ property $ \(c::Config) ->
    Right c == eitherDecode (encode c)

  it "should succeed on good case" $ do
    let goodData = "{ \"http_port\": 9999 }"
    let res = unCompactParser <$> eitherDecode goodData
    res `shouldBe` (eitherDecode goodData :: Either String Config)
    res `shouldBe` (Right (Config 9999))

  it "should fail on bad case" $ do
    let badData = "{ \"httpPort\": 9999 }"
    let res :: Either String Config
        res = unCompactParser <$> eitherDecode badData
    eitherDecode badData `shouldBe` Right (Config 8080)
    res `shouldBe`  Left "Error in $: parser is not compact!Patch {patchOperations = [Rem {changePointer = Pointer {pointerPath = [OKey \"httpPort\"]}},Add {changePointer = Pointer {pointerPath = [OKey \"http_port\"]}, changeValue = Number 8080.0}]}"
