{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Program where

import           Node           hiding ( Expr(Hole) )
import qualified Node
import           PrettyPrinting
import           Optics
import           Prettyprinter

data Program
    = Program { expr :: Node.Expr, whereClause :: WhereClause }
    | Hole HoleContents
    deriving ( Show, Eq )

makeFieldLabelsWith noPrefixFieldLabels ''Program

prettyProgram :: Program -> Doc HoleAnnotation
prettyProgram Program{..} = prettyExpr expr <> prettyWhereClause whereClause
prettyProgram (Hole contents) = prettyHoleContents contents