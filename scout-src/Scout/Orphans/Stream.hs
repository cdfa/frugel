{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Scout.Orphans.Stream where

import Frugel.CstrSite

import Text.Megaparsec as Megaparsec

deriving instance (Ord (Token s), Ord e) => Ord (Megaparsec.ParseError s e)

deriving newtype instance Ord n => Stream (ACstrSite n)
