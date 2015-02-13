{-# LANGUAGE MagicHash, OverloadedStrings, TemplateHaskell #-}

module Main where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Monoid ((<>), Monoid (mconcat, mempty))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.BufferBuilder.Json
import Data.BufferBuilder.Aeson
import qualified Data.Attoparsec.ByteString as Atto
import Data.Scientific
import qualified Data.Aeson.Parser as JsonParse
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import AesonQuickCheck ()
import Debug.Trace

ae expected actual = assertEqual expected (fromString expected) actual

decodeJsonFragment :: Aeson.FromJSON j => BS.ByteString -> Maybe j
decodeJsonFragment str = case parsed of
    Right r -> case Aeson.fromJSON r of
        Aeson.Success a -> Just a
        _              -> Nothing
    _ -> Nothing
  where
    parsed = Atto.parseOnly JsonParse.value' str

case_serialize_simple_things = do
    ae "false" (encodeJson $ Aeson.Bool False)
    ae "true" (encodeJson $ Aeson.Bool True)
    ae "null" (encodeJson $ Aeson.Null)
    ae "9" (encodeJson $ Aeson.Number 9)
    ae "-9" (encodeJson $ Aeson.Number (-9))

prop_matches_aeson :: Aeson.Value -> Bool
prop_matches_aeson value =
    let encoded = encodeJson value
        decoded = decodeJsonFragment encoded
    in if decoded == Just value
        then True
        else trace (show (value, encoded, decoded)) False

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
