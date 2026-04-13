{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Types and functions relating to hash values.
--
module Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    , mockHash
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..)
    )
import Data.ByteArray
    ( ByteArrayAccess
    )
import Data.ByteString
    ( ByteString
    )
import Data.Data
    ( Data
    )
import Data.Hashable
    ( Hashable (hash)
    )
import GHC.Generics
    ( Generic
    )
import GHC.TypeLits
    ( Symbol
    )
import NoThunks.Class
    ( NoThunks (..)
    )
import Quiet
    ( Quiet (..)
    )

import qualified Data.ByteString.Char8 as B8

newtype Hash (tag :: Symbol) = Hash { getHash :: ByteString }
    deriving stock (Data, Generic, Eq, Ord)
    deriving newtype (ByteArrayAccess)
    deriving (Read, Show) via (Quiet (Hash tag))
    deriving anyclass (NFData, Hashable)

instance NoThunks (Hash tag)

-- | Constructs a hash that is good enough for testing.
--
mockHash :: Hashable a => a -> Hash whatever
mockHash = Hash . B8.pack . show . hash
