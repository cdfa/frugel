{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Scout.Orphans.MultiSet where

import Data.MultiSet

instance One (MultiSet a) where
    type OneItem (MultiSet a) = a
    one = singleton
