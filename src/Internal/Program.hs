{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Program where

import           Node           hiding ( Expr, whereClause )
import qualified Node
import           PrettyPrinting
import           Optics
import           Prettyprinter
import           Internal.Meta
                 ( Meta, ProgramMeta(standardMeta), defaultProgramMeta )
import           Data.Has

data Program
    = Program { meta        :: ProgramMeta
              , expr        :: Node.Expr
              , whereClause :: WhereClause
              }
    | Hole ProgramMeta HoleContents
    deriving ( Show, Eq, Generic, Has ProgramMeta )

makeFieldLabelsWith noPrefixFieldLabels ''Program

instance Has Meta Program where
    getter p = standardMeta $ getter p
    modifier = over (programMeta % #standardMeta)

programMeta :: Lens' Program ProgramMeta
programMeta = hasLens

program :: Node.Expr -> WhereClause -> Program
program = Program defaultProgramMeta

prettyProgram :: Program -> Doc Annotation
prettyProgram Program{..} = prettyExpr expr <> prettyWhereClause whereClause
prettyProgram (Hole _ contents) = prettyHoleContents contents