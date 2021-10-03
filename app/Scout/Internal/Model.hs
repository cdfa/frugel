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

data Model
    = Model { editableDataVersion :: Integer
            , cursorOffset :: Int
            , program :: Program
            , errors :: [Error]
            , evaluationOutput :: EvaluationOutput
            , focusedNodeValueIndex :: Int
            , partiallyEvaluated :: Bool
            , fuelLimit :: Int
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
