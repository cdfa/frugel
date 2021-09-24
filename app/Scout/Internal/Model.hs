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
    = Model { version :: Integer
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
