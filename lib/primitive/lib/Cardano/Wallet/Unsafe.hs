{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2021 IOHK
-- License: Apache-2.0
--
-- Conversion functions which don't have error handling. These are quite
-- convenient to use for jobs like testing, debugging, etc.
--
-- But these "unsafe" functions should not be used in application code, unless
-- it's certain that the error case will never happen.

module Cardano.Wallet.Unsafe
    ( unsafeFromHex
    ) where

import Prelude

import Data.ByteArray
    ( ByteArray
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    )
import Data.ByteString
    ( ByteString
    )
import GHC.Stack
    ( HasCallStack
    )

-- | Take the right side of an 'Either' value. Crash badly if it was a left.
unsafeRight :: (HasCallStack, Show e) => Either e a -> a
unsafeRight = either (error . show) id

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: forall b. (HasCallStack, ByteArray b) => ByteString -> b
unsafeFromHex = unsafeRight . convertFromBase @ByteString @b Base16
