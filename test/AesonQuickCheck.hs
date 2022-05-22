{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AesonQuickCheck where

import Data.Aeson
import Test.QuickCheck
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific
import Data.Hashable (Hashable)
import Control.Applicative


newtype ArbObject = ArbObject { unArbObject :: Object }

-- bleh hack
instance Arbitrary Text where
    arbitrary = fmap T.pack $ arbitrary
    shrink txt = fmap T.pack $ shrink $ T.unpack txt

#if !MIN_VERSION_aeson(2, 0, 3)
arbitraryArray :: (Arbitrary a) => Gen [a]
arbitraryArray = sized $ \s -> do
    (Positive len) <- arbitrary
    vector $ min s (len `mod` 10)

arbitraryObject :: (Hashable k, Eq k, Arbitrary k, Arbitrary v) => Gen (HashMap k v)
arbitraryObject = fmap HashMap.fromList arbitraryArray

shrinkHashMap hashMap = fmap HashMap.fromList (shrink (HashMap.toList hashMap))

instance Arbitrary ArbObject where
    arbitrary = sized $ \s -> let s' = max 1 (s `div` 10) in
        ArbObject <$> resize s' arbitraryObject

instance Arbitrary Value where
    arbitrary = sized $ \s -> let s' = max 1 (s - 1) in seq s' $ resize s' $ oneof
        [ Object . unArbObject <$> arbitrary
        , Array . V.fromList <$> arbitraryArray
        , String <$> arbitrary
        , Number <$> arbitrary
        , Bool   <$> arbitrary
        , return Null
        ]

    shrink e = case e of
        Object x -> Object <$> shrinkHashMap x
        Array x  -> Array <$> shrink x
        String _ -> []
        Number _ -> []
        Bool _   -> []
        Null     -> []
#endif

instance Arbitrary Scientific where
    arbitrary = do
        val <- arbitrary :: Gen Int
        return $ scientific (fromIntegral val) 1
    shrink val = do
        val' <- shrink (floor val)
        return $ scientific val' 0

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = V.fromList <$> arbitrary
    shrink = fmap V.fromList . shrink . V.toList
