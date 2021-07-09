{-# OPTIONS_GHC -Wno-deprecations #-}

module Data.Hidden where

import Data.Data
import Data.GenValidity

import Text.Show

newtype Hidden a = Hidden a
    deriving ( Typeable )

instance Eq (Hidden a) where
    _ == _ = True -- What is hidden should not matter

instance Show (Hidden a) where
    show _ = "Hidden"

instance Ord (Hidden a) where
    _ <= _ = True

instance Typeable a => Data (Hidden a) where
    gunfold _ _ _ = undefined
    toConstr x = mkConstr (dataTypeOf x) "Hidden" [] Prefix
    dataTypeOf _ = mkDataType "Data.Hidden.Hidden" []

instance Validity (Hidden a) where
    validate _ = valid
