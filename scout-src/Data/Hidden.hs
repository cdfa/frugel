{-# OPTIONS_GHC -Wno-deprecations #-}

module Data.Hidden where

import Data.Data
import Data.GenValidity

import Text.Show

-- | A type that is not interacted with in any other way than through the constructor.
-- | The defined instances may break laws, but are well-defined (no undefined should show up).
-- | Instances of types that include this type are only legal for values that do not include a Hidden value
newtype Hidden a = Hidden a
    deriving ( Typeable )

instance Eq (Hidden a) where
    _ == _ = True -- What is hidden should not matter

-- Breaks law that it produces a valid Haskell expression
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
