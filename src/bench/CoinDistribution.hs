{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoinDistribution where

import Cardano.CoinSelection
    ( CoinMap )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import GHC.Generics
    ( Generic )
import Internal.Coin
    ( Coin )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )

import qualified Data.Foldable as F
import qualified Internal.Coin as Coin
import qualified Data.Map.Strict as Map

data Bucket = Bucket
    { bucketIndex :: Natural
    , bucketCount :: Natural
    , bucketMinBound :: Coin
    , bucketMaxBound :: Coin
    }
    deriving (Eq, Generic, Ord, Show)

newtype CoinDistribution = CoinDistribution
    { unCoinDistribution :: Map Natural Natural }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet CoinDistribution)

instance Semigroup CoinDistribution where
    d0 <> d1 = wrap $ Map.unionWith (+) (unwrap d0) (unwrap d1)

instance Monoid CoinDistribution where
    mempty = wrap Map.empty

wrap :: Map Natural Natural -> CoinDistribution
wrap = CoinDistribution

unwrap :: CoinDistribution -> Map Natural Natural
unwrap = unCoinDistribution

fromCoinList :: [Coin] -> CoinDistribution
fromCoinList coins = F.foldMap singleton coins

lookupBucket :: CoinDistribution -> Natural -> Bucket
lookupBucket d i = Bucket {..}
  where
    bucketIndex = i
    bucketMinBound
        | i == 0 = Coin.zero
        | otherwise = Coin.coinFromNatural $ 10 ^ (i - 1)
    bucketMaxBound = Coin.coinFromNatural $ 10 ^ i - 1
    bucketCount = fromMaybe 0 $ Map.lookup i $ unwrap d

toBucketList :: CoinDistribution -> [Bucket]
toBucketList d = [lookupBucket d i | i <- [0 .. magnitude d]]

singleton :: Coin -> CoinDistribution
singleton = go 0 . Coin.coinToNatural
  where
    go !bucket !coin
        | coin == 0 = CoinDistribution $ Map.singleton bucket 1
        | otherwise = go (bucket + 1) (coin `div` 10)

magnitude :: CoinDistribution -> Natural
magnitude d = maybe 0 fst $ Map.lookupMax $ unwrap d
