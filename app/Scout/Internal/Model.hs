{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Internal.Model where

import Optics.Extra.Scout

import Scout

-- It would be nicer to split the data into "editable data" that contains a version which is automatically updated, but at the moment FocusedNodeValueIndexAction and ChangeSelectedNodeValueTreeDepth are the only action where the version is updated manually
data Model
    = Model { editableDataVersion :: Integer
            , partiallyEvaluated :: Bool
            , cursorOffset :: Int
            , program :: Program
            , errors :: [Error]
            , fuelLimit :: Int
            , selectedNodeEvaluationIndex :: Int
            , selectedNodeValueRenderDepth :: Int
            , contextRenderDepth :: Int
            , definitionsViewCollapsed :: Bool
              -- evaluationOutput being last is VERY IMPORTANT, because focusedNodeEvaluations may contain non-terminating computations and (==) will not terminate if no other difference is found in any previous field
              -- At the moment, partiallyEvaluated also changes when evaluationOutput is set to a value that may not terminate
              -- Using a breadth-first implementation of Eq would be a more elegant solution
            , evaluationOutput :: EvaluationOutput
            }
    deriving ( Show, Eq )

data EvaluationOutput
    = EvaluationOutput { evaluated :: Program
                       , focusedNodeEvaluations :: Seq FocusedNodeEvaluation
                       }
    deriving ( Show, Eq )

makeFieldLabelsNoPrefix ''Model

makeFieldLabelsNoPrefix ''EvaluationOutput

instance LabelOptic "selectedNodeEvaluation" An_AffineTraversal Model Model FocusedNodeEvaluation FocusedNodeEvaluation where
    labelOptic = atraversal matcher updater
      where
        matcher model@Model{..}
            = matching (selectedNodeEvaluation' selectedNodeEvaluationIndex)
                       model
        updater model@Model{..}
            = flip (set $ selectedNodeEvaluation' selectedNodeEvaluationIndex)
                   model
        selectedNodeEvaluation' i
            = #evaluationOutput
            % #focusedNodeEvaluations
            % (ix i `afailing'` _last)
