{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Internal.Model where

import Control.Concurrent

import Optics.Extra.Scout

import Scout

data Model
    = Model { cursorOffset :: Int
            , program :: Program
            , errors :: [Error]
            , evaluationOutput :: EvaluationOutput
            , focusedNodeValueIndex :: Int
            , evalThreadId :: Maybe ThreadId
            , fuelLimit :: Int
            }
    deriving ( Show, Eq )

data EvaluationOutput
    = EvaluationOutput { evaluated :: Program, focusedNodeValues :: Seq Node }
    deriving ( Show, Eq )

makeFieldLabelsWith noPrefixFieldLabels ''Model

makeFieldLabelsWith noPrefixFieldLabels ''EvaluationOutput
