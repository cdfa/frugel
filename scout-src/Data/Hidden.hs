{-# OPTIONS_GHC -Wno-deprecations #-}

module Data.Hidden where

import Data.Data
import Data.GenValidity

import Text.Show

-- | A type that is not interacted with in any other way than through the constructor.
-- | This may be useful for derived data would otherwise prevent the definition of some instances for a data structure containing the data.
-- | The defined instances may be ill-defined or break laws.
-- | It is the programmers responsibility to ensure that functions constrained on these instances are not applied to values containing hidden data.
newtype Hidden a = Hidden a
    deriving ( Typeable )

instance Eq (Hidden a) where
    _ == _ = True -- What is hidden should not matter

instance Show (Hidden a) where
    show _ = undefined

instance Ord (Hidden a) where
    _ <= _ = True

instance Typeable a => Data (Hidden a) where
    gunfold _ _ _ = undefined
    toConstr x = mkConstr (dataTypeOf x) "Hidden" [] Prefix
    dataTypeOf _ = mkDataType "Data.Hidden.Hidden" []

instance Validity (Hidden a) where
    validate _ = valid
