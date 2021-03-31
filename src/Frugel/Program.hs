{-# LANGUAGE RecordWildCards #-}

module Frugel.Program
    ( module Frugel.Program
    , Program(Program, ProgramCstrSite)
    , programMeta
    ) where

import           Frugel.Internal.Program
import           Frugel.Meta
import           Frugel.Node             hiding ( whereClause )
import           Frugel.PrettyPrinting

import           Prettyprinter

program :: Expr -> WhereClause -> Program
program = Program defaultProgramMeta

prettyProgram :: Program -> Doc Annotation
prettyProgram Program{..} = prettyExpr expr <> prettyWhereClause whereClause
prettyProgram (ProgramCstrSite _ contents) = prettyCstrMaterials contents