-- Writing anything in here is inconvenient at the moment, because floskell removes the package-qualified import (See https://github.com/ennocramer/floskell/issues/60)
{-# LANGUAGE PackageImports #-}

module Optics.External ( module External ) where

-- Using PackageImports until https://github.com/commercialhaskell/stack/issues/5077 is solved
import "optics" Optics as External
