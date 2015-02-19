# buffer-builder-aeson

A very fast [buffer-builder](http://hackage.haskell.org/package/buffer-builder)-based JSON encoder for [Aeson](http://hackage.haskell.org/package/aeson).

The whole library is a single [Data.BufferBuilder.Json.ToJson](https://github.com/chadaustin/buffer-builder/blob/40361f929868ff62ff0cdafc6bdb0fe4a57d0a16/Data/BufferBuilder/Json.hs#L54) instance for [Data.Aeson.Value](http://hackage.haskell.org/package/aeson-0.8.0.2/docs/Data-Aeson.html#t:Value).

[Criterion benchmark](benchmark_results.txt)

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Data.Aeson (object, (.=), Value (..))
import Data.BufferBuilder.Json (encodeJson)
import Data.BufferBuilder.Aeson ()
import qualified Data.ByteString as BS

stuff :: Value
stuff = Array
    [ object
        [ "name" .= ("eggs" :: String)
        , "price" .= (3.141592 :: Double)
        ]
    , object
        [ "name" .= ("cheese" :: String)
        , "price" .= (0.57721 :: Double)
        ]
    ]

main :: IO ()
main = do
    BS.putStrLn $ encodeJson stuff
```
