module Frugel.Meta
    ( module Frugel.Meta
    , Meta(Meta)
    , ExprMeta(ExprMeta)
    , ProgramMeta(ProgramMeta)
    ) where

import           Frugel.Internal.Meta

defaultExprMeta :: ExprMeta
defaultExprMeta
    = ExprMeta { parenthesisLevels = 0, standardMeta = defaultMeta }

defaultProgramMeta :: ProgramMeta
defaultProgramMeta
    = ProgramMeta { standardMeta = defaultMeta, trailingWhitespace = "" }

defaultMeta :: Meta
defaultMeta = Meta { interstitialWhitespace = [] }