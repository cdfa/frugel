module Frugel.Program
    ( module Frugel.Program
    , Program(Program, ProgramCstrSite)
    , programMeta
    ) where

import           Frugel.Internal.Program
import           Frugel.Meta
import           Frugel.Node

program' :: Expr -> WhereClause -> Program
program' = Program defaultProgramMeta