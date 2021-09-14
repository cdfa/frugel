{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.Sized where

import Data.GenValidity

data Sized (s :: Nat) a = Sized { size :: Proxy s, unSized :: a }
    deriving ( Show )

instance Validity a => Validity (Sized s a) where
    validate = validate . unSized
