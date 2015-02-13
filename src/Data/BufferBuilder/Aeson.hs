{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

module Data.BufferBuilder.Aeson () where

import           Control.Monad (when)
import           Data.Aeson (Value (..))
import           Data.BufferBuilder.Json
import qualified Data.BufferBuilder.Utf8 as Utf8Builder
import           Data.Monoid
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Scientific as BB
import qualified Data.Scientific as Scientific
import qualified Data.HashMap.Strict as HashMap

fitsInInt :: Integer -> Bool
fitsInInt i =
    i < fromIntegral (maxBound :: Int) &&
    i > fromIntegral (minBound :: Int)

instance ToJson Value where
    {-# INLINE appendJson #-}
    appendJson val = case val of
        Object o ->
            let f a k v = a <> k .= v
            in appendJson $ HashMap.foldlWithKey' f mempty o
        Array a -> vector a
        String s -> appendJson s
        Number n
            | fitsInInt (Scientific.coefficient n) -> unsafeAppendUtf8Builder $ do
                    Utf8Builder.appendDecimalSignedInt $ fromIntegral $ Scientific.coefficient n
                    let ex = Scientific.base10Exponent n
                    when (ex /= 0) $ do
                        Utf8Builder.appendChar8 'e'
                        Utf8Builder.appendDecimalSignedInt $ Scientific.base10Exponent n
            | otherwise ->
                unsafeAppendBS
                    $ BSL.toStrict
                    $ BB.toLazyByteString
                    $ BB.formatScientificBuilder BB.Fixed Nothing n
        Bool b -> appendJson b
        Null -> appendJson (Nothing :: Maybe Int)
