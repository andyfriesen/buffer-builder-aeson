{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

module Data.BufferBuilder.Aeson () where

import GHC.Base
import GHC.Integer.GMP.Internals
import           Data.Aeson (Value (..))
import           Data.BufferBuilder.Json
import qualified Data.BufferBuilder.Utf8 as Utf8Builder
import           Data.Monoid
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Builder.Scientific as BB
import qualified Data.Scientific as Scientific
import qualified Data.HashMap.Strict as HashMap

-- TODO: this doesn't need to convert the bytestring to strict before appending it
-- there is an appendBSL
slowNumber :: Scientific.Scientific -> JsonBuilder
slowNumber n = unsafeAppendBS
                    $ BSL.toStrict
                    $ BB.toLazyByteString
                    $ BB.formatScientificBuilder BB.Fixed Nothing n
                    
instance ToJson Value where
    {-# INLINE appendJson #-}
    appendJson (Object o) = hashMap o
    appendJson (Array a) = vector a
    appendJson (String s) = appendJson s
    appendJson (Number n) = case Scientific.coefficient n of
        (S# smallcoeff) -> case Scientific.base10Exponent n of
            0 -> appendJson (I# smallcoeff)
            exp' -> unsafeAppendUtf8Builder $ do
                Utf8Builder.appendDecimalSignedInt (I# smallcoeff)
                Utf8Builder.appendChar7 'e'
                Utf8Builder.appendDecimalSignedInt exp'
        _ -> slowNumber n
    appendJson (Bool b) = appendJson b
    appendJson Null = appendNull
