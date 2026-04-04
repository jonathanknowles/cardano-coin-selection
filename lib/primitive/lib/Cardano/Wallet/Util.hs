{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2020-2021 IOHK
-- License: Apache-2.0
--
-- General utility functions.
--
module Cardano.Wallet.Util
    ( internalError
    , mapFirst
    ) where

import Prelude

import Fmt
    ( Builder
    , fmt
    , (+|)
    )
import GHC.Stack
    ( HasCallStack
    )

-- | Calls the 'error' function, which will usually crash the program.
internalError :: HasCallStack => Builder -> a
internalError msg = error $ fmt $ "INTERNAL ERROR: "+|msg

-- | Map a function to the first element of a list. Does nothing if the list is
-- empty.
mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _     [] = []
mapFirst fn (h:q) = fn h:q
