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
    ( mapFirst
    ) where

-- | Map a function to the first element of a list. Does nothing if the list is
-- empty.
mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _     [] = []
mapFirst fn (h:q) = fn h:q
