module Frugel.Program
    ( module Frugel.Program
    , Program(Program, ProgramCstrSite)
    , programMeta
    , programCstrSite'
    ) where

import           Frugel.Internal.Program
import           Frugel.Node

program' :: Expr -> Maybe WhereClause -> Program
program' = Program $ defaultProgramMeta 1