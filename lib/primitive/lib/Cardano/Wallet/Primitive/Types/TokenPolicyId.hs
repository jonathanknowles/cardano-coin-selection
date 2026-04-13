{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Data
    ( Data
    )
import Data.Hashable
    ( Hashable
    )
import Data.Text.Class
    ( FromText (..)
    , ToText (..)
    )
import GHC.Generics
    ( Generic
    )
import Quiet
    ( Quiet (..)
    )

-- | Token policy identifiers, represented by the hash of the monetary policy
-- script.
newtype TokenPolicyId =
    -- | Construct a 'TokenPolicyId' without any validation.
    UnsafeTokenPolicyId { unTokenPolicyId :: Hash "TokenPolicy" }
    deriving stock (Data, Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenPolicyId)
    deriving anyclass Hashable

instance NFData TokenPolicyId

instance ToText TokenPolicyId where
    toText = toText . unTokenPolicyId

instance FromText TokenPolicyId where
    fromText = fmap UnsafeTokenPolicyId . fromText
