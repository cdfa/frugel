{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Program
    ( module Scout.Program
    , Program(Program, ProgramCstrSite)
    , ProgramMeta(ProgramMeta)
    , _Program
    , _ProgramCstrSite
    , program'
    , programMeta
    , programCstrSite'
    , unsafePrettyProgram
    , enumerateValidProgramMeta
    ) where

import qualified Control.Lens    as Lens

import Data.Constrained
import Data.Data
import Data.Data.Lens
import Data.Dynamic
import Data.Dynamic.Lens         as DL
import Data.Has

import Optics.Extra.Scout        as Optics

import Scout.Internal.Program    hiding ( expr, whereClause )
import Scout.Node
import Scout.Orphans.DisplayProjection ()

class (LabelOptic' "exprMeta" An_AffineFold a ExprMeta, Has Meta a, Data a)
    => EvaluatedConstraint a

instance (LabelOptic' "exprMeta" An_AffineFold a ExprMeta, Has Meta a, Data a)
    => EvaluatedConstraint a

-- I think this mess isn't actually even necessary without evaluation observing rendering of focused expressions, because now it's only used in unsafeEvaluateSelectedNodeValue and we don't care about forcing OutOfFuel source expressions and all other expressions in EvaluationStatus are Hidden
allEvaluatedChildren :: ( HasCallStack
                        , LabelOptic' "exprMeta" An_AffineFold a ExprMeta
                        , Has Meta a
                        , Data a
                        )
    => Fold a (Constrained EvaluatedConstraint)
allEvaluatedChildren
    = to Constrained
    % evaluated
    % notElided
    % Optics.cosmosOf (nodeChildren % evaluated % notElided)
  where
    notElided :: AffineFold (Constrained EvaluatedConstraint)
                            (Constrained EvaluatedConstraint)
    notElided
        = filtered $ fromConstrained (not . view (hasLens @Meta % #elided))
    evaluated :: AffineFold (Constrained EvaluatedConstraint)
                            (Constrained EvaluatedConstraint)
    evaluated = filtered $ fromConstrained predicate
      where
        predicate
            :: (LabelOptic' "exprMeta" An_AffineFold a ExprMeta) => a -> Bool
        predicate
            = maybe True (== Evaluated)
            . preview (#exprMeta % #evaluationStatus)

nodeChildren :: HasCallStack
    => Fold (Constrained EvaluatedConstraint) (Constrained EvaluatedConstraint)
nodeChildren = foldVL nodeChildrenVL

-- todo: make constraint polymorphic
-- The Constrained should be either Program or a child node
nodeChildrenVL :: HasCallStack
    => Lens.Fold (Constrained EvaluatedConstraint)
                 (Constrained EvaluatedConstraint)
nodeChildrenVL f = _ConstrainedVL $ Lens.re @Dynamic _Dynamic nodeChildren'
  where
    nodeChildren' d = d <$ case d of
        DL.Dynamic (ProgramCstrSite _ cstrSite) -> traverseCstrSite cstrSite
        DL.Dynamic program -> traverseOf_
            ((#expr % to Constrained)
             `summing` (#whereClause % _Just % to Constrained))
            f
            (program :: Program)
        DL.Dynamic node -> traverseNode node
        DL.Dynamic expr -> traverseExpr expr
        DL.Dynamic def -> traverseExpr def
        DL.Dynamic whereClause -> traverseExpr whereClause
        _ -> error "nodeChildrenVL used on non-Node type"
    traverseCstrSite = traverseOf_ (_CstrSite % folded % _Right) $ \case
        ExprNode e -> f $ Constrained e
        DefNode e -> f $ Constrained e
        WhereNode e -> f $ Constrained e
    traverseNode = \case
        ExprNode e -> traverseExpr e
        DefNode d -> traverseDef d
        WhereNode w -> traverseWhereClause w
    traverseExpr (ExprCstrSite _ cstrSite) = traverseCstrSite cstrSite
    traverseExpr expr
        = Lens.traverseOf_ (uniplate . Lens.to Constrained) f expr
    traverseDef (DefCstrSite _ cstrSite) = traverseCstrSite cstrSite
    traverseDef def = traverseOf_ (#value % to Constrained) f def
    traverseWhereClause (WhereCstrSite _ cstrSite) = traverseCstrSite cstrSite
    traverseWhereClause (WhereClause _ defs) = traverse_ (f . Constrained) defs
