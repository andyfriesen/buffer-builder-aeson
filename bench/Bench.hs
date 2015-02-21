{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.DeepSeq (force)
import           Criterion
import           Criterion.Main
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.BufferBuilder.Json as Json
import qualified Data.BufferBuilder.Aeson ()
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UnboxedVector

assumeSuccess :: Either a b -> b
assumeSuccess (Right r) = r
assumeSuccess _ = error "assumeSuccess"

main :: IO ()
main = do
    content <- BS.readFile "test.json"
    let lazyContent = force $ BSL.fromChunks [content]

    let parsedUserList :: [Aeson.Value]
        Just parsedUserList = Aeson.decode lazyContent

    let compareBench name !value =
            bgroup name
                [ bench "bufferbuilder" $ nf Json.encodeJson value
                , bench "aeson"         $ nf Aeson.encode value
                ]

    defaultMain
        [ compareBench "list bool" $ Aeson.Array $ Vector.fromList $ fmap Aeson.Bool $ replicate 65536 False
        , compareBench "list null" $ Aeson.Array $ Vector.replicate 65536 Aeson.Null
        , compareBench "list empty object" $ Aeson.Array $ Vector.replicate 65536 $ Aeson.object []
        , compareBench "list empty array" $ Aeson.Array $ Vector.replicate 65536 $ Aeson.Array Vector.empty
        , compareBench "list string" $ Aeson.Array $ Vector.fromList $ fmap (Aeson.String . Text.pack . show) ([0..65535] :: [Int])
        , compareBench "list int" $ Aeson.Array $ Vector.fromList $ fmap (Aeson.Number . fromIntegral) ([0..65535] :: [Int])
        , compareBench "hash string" $ Aeson.object $ fmap (\e -> (Text.pack $ show e, Aeson.Null)) ([0..65535] :: [Int])

        , compareBench "list record" parsedUserList
        , bench "intvector" $ nf Json.encodeJson (UnboxedVector.fromList $! [0..65535] :: UnboxedVector.Vector Int)
        ]
