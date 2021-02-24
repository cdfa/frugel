module Prelude
    ( module Relude
    , module Relude.Extra.Newtype
    , testPrettyW
    ) where

import           Relude
import           Relude.Extra.Newtype
import           Prettyprinter
import           Prettyprinter.Render.Text

testPrettyW :: Int -> Doc ann -> IO String
testPrettyW w doc
    = error $ renderStrict (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine w 1 }
