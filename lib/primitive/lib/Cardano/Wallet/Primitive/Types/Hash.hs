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
    ( Hashable
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

newtype Hash (tag :: Symbol) = Hash { getHash :: ByteString }
    deriving stock (Data, Generic, Eq, Ord)
    deriving newtype (ByteArrayAccess)
    deriving (Read, Show) via (Quiet (Hash tag))
    deriving anyclass (NFData, Hashable)

instance NoThunks (Hash tag)
