{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Scout.Model ( module Scout.Model, Model(Model) ) where

import Data.MultiSet  hiding ( map )

import qualified Frugel

import Optics.Extra.Scout

import Scout
import Scout.Internal.Model

-- Assumes program terminates
initialModel :: Program -> Model
initialModel programCstrSite
    = fromFrugelModel
        Model { editableDataVersion = 0
              , fuelLimit = initialFuelLimit
              , focusedNodeValueIndex = 0
              , errors = []
              , partiallyEvaluated = False
              , evaluationOutput = EvaluationOutput { evaluated = program
                                                    , focusedNodeValues = mempty
                                                    }
              , ..
              }
        frugelModel
  where
    frugelModel@Frugel.Model{..}
        = Frugel.prettyPrint -- pretty print twice, because program may not be fully parsed (and then it's only parsed but not pretty-printed)
        . Frugel.prettyPrint
        $ Frugel.initialModel programCstrSite

-- Currently, the largest limiting factor on this is that rendering big partially evaluated programs
-- Otherwise it could be 20, which is still to low for real-world programs. To make partial evaluation useful for those, the editor could present iteratively further evaluated programs
initialFuelLimit :: Int
initialFuelLimit = 10

-- Assumes program terminates
fromFrugelModel :: Model -> Frugel.Model Program -> Model
fromFrugelModel = partialFromFrugelModel Infinity

partialFromFrugelModel :: Limit -> Model -> Frugel.Model Program -> Model
partialFromFrugelModel
    fuel
    scoutModel@Model{editableDataVersion, focusedNodeValueIndex}
    Frugel.Model{..}
    = Model { editableDataVersion = editableDataVersion + 1
            , errors = map fromFrugelError errors
                  ++ map (uncurry $ flip EvaluationError)
                         (toOccurList evalErrors)
            , partiallyEvaluated = case fuel of
                  Only _ -> True
                  Infinity -> False
            , evaluationOutput = EvaluationOutput { .. }
            , fuelLimit = fuelLimit scoutModel
            , ..
            }
  where
    (evaluated, (evalErrors, focusedNodeValues))
        = runEval (Just cursorOffset) fuel evalProgram program

updateWithFrugelErrors :: [Frugel.Error Program] -> Model -> Model
updateWithFrugelErrors newErrors = over #errors $ \oldErrors ->
    rights (map matchFrugelError oldErrors) ++ map fromFrugelError newErrors

toFrugelModel :: Model -> Frugel.Model Program
toFrugelModel Model{..}
    = Frugel.Model { errors = toListOf (folded % _FrugelError) errors, .. }

hideSelectedNodeValue :: Model -> Model
hideSelectedNodeValue
    = (#partiallyEvaluated .~ True)
    . (#selectedNodeValue .~ evaluationPlaceHolder)
  where
    evaluationPlaceHolder
        = ExprNode . exprCstrSite' . toCstrSite . one $ Left "Evaluating..."