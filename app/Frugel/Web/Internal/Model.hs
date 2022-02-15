{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Web.Internal.Model where

import Optics.Extra.Scout

import Scout        hiding ( EvaluationStatus )

-- It would be nicer to split the data into "editable data" that contains a version which is automatically updated, but at the moment FocusedNodeValueIndexAction and ChangeSelectedNodeValueTreeDepth are the only action where the version is updated manually
data Model
    = Model { editableDataVersion :: Integer
            , evaluationStatus :: EvaluationStatus
            , cursorOffset :: Int
            , program :: Program
            , errors :: [Error]
            , showHelp :: Bool
            , fuelLimit :: Int
            , limitEvaluationByDefault :: Bool
            , selectedNodeEvaluationIndex :: Int
            , mainExpressionRenderDepth :: Int
            , selectedNodeValueRenderDepth :: Int
            , contextRenderDepth :: Int
            , definitionsViewCollapsed :: Bool
              -- evaluationOutput being last is VERY IMPORTANT, because focusedNodeEvaluations may contain non-terminating computations and (==) will not terminate if no other difference is found in any previous field
              -- At the moment, partiallyEvaluated also changes when evaluationOutput is set to a value that may not terminate
              -- Using a breadth-first implementation of Eq would be a more elegant solution
            , evaluationOutput :: EvaluationOutput
            }
    deriving ( Show, Eq )

data EvaluationStatus = Evaluated | PartiallyEvaluated | Aborted String
    deriving ( Show, Eq )

data EvaluationOutput
    = EvaluationOutput { evaluated :: Program
                       , focusedNodeEvaluations :: Seq FocusedNodeEvaluation
                       }
    deriving ( Show, Eq )

makeFieldLabelsNoPrefix ''Model

makeFieldLabelsNoPrefix ''EvaluationOutput

instance LabelOptic "selectedNodeEvaluation" An_AffineTraversal Model Model FocusedNodeEvaluation FocusedNodeEvaluation where
    labelOptic = atraversal matcher setter
      where
        matcher model@Model{..}
            = matching (selectedNodeEvaluation' selectedNodeEvaluationIndex)
                       model
        setter model@Model{..}
            = flip (set $ selectedNodeEvaluation' selectedNodeEvaluationIndex)
                   model
        selectedNodeEvaluation' i
            = #evaluationOutput
            % #focusedNodeEvaluations
            % (ix i `gfailing` _last)
