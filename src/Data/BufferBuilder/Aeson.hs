
module Data.BufferBuilder.Aeson where

import           Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import Data.BufferBuilder.Json
import Data.Monoid
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Scientific as BB
import qualified Data.Scientific as Scientific
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import Data.Int (Int32)

maxDecimalNumber = Scientific.scientific (fromIntegral (maxBound :: Int32)) 0

instance ToJson Value where
    {-# INLINE appendJson #-}
    appendJson val = case val of
        Object o ->
            let f a k v = a <> k .= v
            in appendJson $ HashMap.foldlWithKey' f mempty o
        Array a -> vector a
        String s -> appendJson s
        Number n
            | Scientific.base10Exponent n < 0 || (abs n) >= maxDecimalNumber ->
                unsafeAppendBS
                    $ BSL.toStrict
                    $ BB.toLazyByteString
                    $ BB.formatScientificBuilder BB.Fixed Nothing n
            | otherwise ->
                appendJson (floor n :: Int)
        Bool b -> appendJson b
        Null -> appendJson (Nothing :: Maybe Int)
