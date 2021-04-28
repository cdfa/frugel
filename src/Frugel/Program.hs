module Frugel.Program
    ( module Frugel.Program
    , Program(Program, ProgramCstrSite)
    , programMeta
    ) where

import           Frugel.Internal.Program
import           Frugel.Node

program' :: Expr -> Maybe WhereClause -> Program
program' = Program defaultProgramMeta