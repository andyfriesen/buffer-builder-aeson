{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

module Data.BufferBuilder.Aeson () where

import GHC.Base
import GHC.Integer.GMP.Internals
import           Data.Aeson (Value (..))
import           Data.BufferBuilder.Json (ToJson (..), nullValue, unsafeAppendBS, unsafeAppendUtf8Builder)
import qualified Data.BufferBuilder.Json as Json
import qualified Data.BufferBuilder.Utf8 as Utf8Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Scientific as BB
import qualified Data.Scientific as Scientific

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (foldrWithKey)
import Data.Aeson.Key (toText)
#endif

-- TODO: this doesn't need to convert the bytestring to strict before appending it
-- there is an appendBSL
slowNumber :: Scientific.Scientific -> Json.Value
slowNumber n = unsafeAppendBS
                    $ BSL.toStrict
                    $ BB.toLazyByteString
                    $ BB.formatScientificBuilder BB.Fixed Nothing n

instance ToJson Value where
    {-# INLINE toJson #-}
#if MIN_VERSION_aeson(2,0,0)
    toJson (Object o) = toJson $ foldrWithKey
                            (\k v built -> (toText k Json..= (toJson v)) <> built)
                            mempty
                            o
#else
    toJson (Object o) = toJson o
#endif
    toJson (Array a) = toJson a
    toJson (String s) = toJson s
    toJson (Number n) = case Scientific.coefficient n of
        (S# smallcoeff) -> case Scientific.base10Exponent n of
            0 -> toJson (I# smallcoeff)
            exp' -> unsafeAppendUtf8Builder $ do
                Utf8Builder.appendDecimalSignedInt (I# smallcoeff)
                Utf8Builder.appendChar7 'e'
                Utf8Builder.appendDecimalSignedInt exp'
        _ -> slowNumber n
    toJson (Bool b) = toJson b
    toJson Null = nullValue
