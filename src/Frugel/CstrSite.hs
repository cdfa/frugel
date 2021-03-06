{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.CstrSite where

import qualified Control.Lens as Lens
import Control.Lens.Plated

import Data.Data
import Data.Data.Lens

import Optics

type family NodeOf a :: Type

class (NodePrism a, CstrSiteNode a) => IsNode a

class NodePrism a where
    nodePrism :: Prism' (NodeOf a) a

class CstrSiteNode a where
    setCstrSite :: ACstrSite (NodeOf a) -> a -> a
    _NodeCstrSite :: AffineTraversal' a (ACstrSite (NodeOf a))

newtype ACstrSite n = CstrSite (Seq (Either Char n))
    deriving ( Eq, Ord, Show, Generic, Data )
    deriving newtype ( One, IsList, Semigroup, Monoid )

type instance NodeOf (ACstrSite a) = a

makePrisms ''ACstrSite

instance Cons (ACstrSite n) (ACstrSite n) (Either Char n) (Either Char n) where
    _Cons = _CstrSite % _Cons % aside (re _CstrSite)

instance Snoc (ACstrSite n) (ACstrSite n) (Either Char n) (Either Char n) where
    _Snoc = _CstrSite % _Snoc % swapped % aside (re _CstrSite) % swapped

instance Eq n => AsEmpty (ACstrSite n)

-- concatCstrSite :: [CstrSite] -> CstrSite
-- concatCstrSite = CstrSite . join . fromList . map (view _CstrSite)
cstrSiteCount :: Data n => ACstrSite n -> Int
cstrSiteCount = Lens.lengthOf $ cosmosOf uniplate
