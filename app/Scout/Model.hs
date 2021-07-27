{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Scout.Model ( module Scout.Model, Model(Model) ) where

import Data.MultiSet  hiding ( map )

import qualified Frugel

import Optics.Extra

import Scout
import Scout.Internal.Model

-- Assumes program terminates
initialModel :: Program -> Model
initialModel
    = unsafeFromFrugelModel
    . Frugel.prettyPrint -- pretty print twice, because program may not be fully parsed (and then it's only parsed but not pretty-printed)
    . Frugel.prettyPrint
    . Frugel.initialModel

frugelModel :: Lens' Model (Frugel.Model Program)
frugelModel = lens toFrugelModel updateWithFrugelModel

-- Only use when program does not change
-- New program is not evaluated
updateWithFrugelModel :: Model -> Frugel.Model Program -> Model
updateWithFrugelModel
    Model{errors = oldErrors, cursorOffset = _, program = _, ..}
    Frugel.Model{errors = newErrors, cursorOffset, program}
    = Model { errors = rights (map matchFrugelError oldErrors)
                  ++ map fromFrugelError newErrors
            , ..
            }

-- Assumes program terminates
unsafeFromFrugelModel :: Frugel.Model Program -> Model
unsafeFromFrugelModel Frugel.Model{..}
    = Model { errors = map fromFrugelError errors
                  ++ map (uncurry $ flip EvaluationError)
                         (toOccurList evalErrors)
            , evalThreadId = Nothing
            , ..
            }
  where
    (evaluated, evalErrors) = runEval program

toFrugelModel :: Model -> Frugel.Model Program
toFrugelModel Model{..}
    = Frugel.Model { errors = toListOf (folded % _FrugelError) errors, .. }
