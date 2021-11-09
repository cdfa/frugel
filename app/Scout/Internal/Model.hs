{-# LANGUAGE DataKinds #-}
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
            , focusedNodeValueIndex :: Int
            , fuelLimit :: Int
            , selectedNodeValueRenderDepth :: Int
              -- evaluationOutput being last is VERY IMPORTANT, because focusedNodeValues may contain non-terminating computations and (==) will not terminate if no other difference is found in any previous field
              -- At the moment, partiallyEvaluated also changes when evaluationOutput is set to a value that may not terminate
              -- Using a breadth-first implementation of Eq would be a more elegant solution
            , evaluationOutput :: EvaluationOutput
            }
    deriving ( Show, Eq )

data EvaluationOutput
    = EvaluationOutput { evaluated :: Program, focusedNodeValues :: Seq Node }
    deriving ( Show, Eq )

makeFieldLabelsNoPrefix ''Model

makeFieldLabelsNoPrefix ''EvaluationOutput

instance LabelOptic "selectedNodeValue" An_AffineTraversal Model Model Node Node where
    labelOptic = atraversal matcher updater
      where
        matcher model@Model{..}
            = matching (selectedNodeValue' focusedNodeValueIndex) model
        updater model@Model{..}
            = flip (set $ selectedNodeValue' focusedNodeValueIndex) model
        selectedNodeValue' :: Int -> AffineTraversal' Model Node
        selectedNodeValue' i
            = #evaluationOutput % #focusedNodeValues % (ix i `afailing'` _last)
