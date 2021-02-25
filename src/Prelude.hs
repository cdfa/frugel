{-# OPTIONS_GHC -Wno-unused-imports #-} -- because exporting unused qualified imports

module Prelude
    ( module Prelude
    , module Relude
    , module Relude.Extra.Newtype
    ) where

import           Relude                    hiding ( abs, init )
import qualified Relude                    ( abs, init )
import           Relude.Extra.Newtype
import           Prettyprinter
import           Prettyprinter.Render.Text

testPrettyW :: Int -> Doc ann -> IO String
testPrettyW w doc
    = error $ renderStrict (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine w 1 }

-- Copied from the Agda package
listCase :: b -> (a -> [a] -> b) -> [a] -> b
listCase n _ [] = n
listCase _ c (x : xs) = c x xs

-- Copied from the Agda package
-- | Groups a list into alternating chunks of 'Left' and 'Right' values
groupByEither :: [Either a b] -> [Either [a] [b]]
groupByEither = listCase [] (go . init)
  where
    go :: Either [a] [b] -> [Either a b] -> [Either [a] [b]]
    go acc [] = [ adjust acc ]
    -- match: next value can be tacked onto the accumulator
    go (Left acc) (Left a : abs) = go (Left $ a : acc) abs
    go (Right acc) (Right b : abs) = go (Right $ b : acc) abs
    -- mismatch: switch the accumulator to the other mode
    go acc (ab : abs) = adjust acc : go (init ab) abs
    adjust = bimap reverse reverse
    init = bimap pure pure
