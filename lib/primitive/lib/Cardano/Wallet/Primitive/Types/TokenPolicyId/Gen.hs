module Cardano.Wallet.Primitive.Types.TokenPolicyId.Gen
    (
    -- * Generators and shrinkers
      genTokenPolicyId
    , genTokenPolicyIdLargeRange
    , shrinkTokenPolicyId

    -- * Test values
    , testTokenPolicyIds

    -- * Creation of test values
    , mkTokenPolicyId

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Data.Word
    ( Word8
    )
import Test.QuickCheck
    ( Gen
    , elements
    , sized
    , vector
    )

import qualified Data.ByteString as BS


--------------------------------------------------------------------------------
-- Token policy identifiers chosen from a range that depends on the size
-- parameter
--------------------------------------------------------------------------------

genTokenPolicyId :: Gen TokenPolicyId
genTokenPolicyId = sized $ \n -> elements $ take (max 1 n) testTokenPolicyIds

shrinkTokenPolicyId :: TokenPolicyId -> [TokenPolicyId]
shrinkTokenPolicyId i
    | i == simplest = []
    | otherwise = [simplest]
  where
    simplest = case testTokenPolicyIds of (x:_) -> x; [] -> error "impossible"

--------------------------------------------------------------------------------
-- Token policy identifiers chosen from a large range (to minimize the risk of
-- collisions)
--------------------------------------------------------------------------------

genTokenPolicyIdLargeRange :: Gen TokenPolicyId
genTokenPolicyIdLargeRange = UnsafeTokenPolicyId . Hash . BS.pack <$> vector 28

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

testTokenPolicyIds :: [TokenPolicyId]
testTokenPolicyIds = mkTokenPolicyId <$> [0 .. 15]

mkTokenPolicyId :: Word8 -> TokenPolicyId
mkTokenPolicyId w =
    UnsafeTokenPolicyId . Hash . BS.pack $
        replicate tokenPolicyIdLengthBytes (fromIntegral w)

tokenPolicyIdLengthBytes :: Int
tokenPolicyIdLengthBytes = 28
