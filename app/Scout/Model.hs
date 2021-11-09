{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Scout.Model ( module Scout.Model, Model(Model) ) where

import Data.MultiSet  hiding ( map )

import qualified Frugel

import Optics.Extra.Scout

import Scout
import Scout.Internal.Model

initialModel :: Program -> Model
initialModel p
    = Model { editableDataVersion = 0
            , fuelLimit = initialFuelLimit
            , focusedNodeValueIndex = 0
            , errors = []
            , partiallyEvaluated = False
            , selectedNodeValueRenderDepth = 10
            , evaluationOutput = EvaluationOutput { evaluated = program'
                                                        evaluationPlaceHolder
                                                        Nothing
                                                  , focusedNodeValues = mempty
                                                  }
            , ..
            }
  where
    Frugel.Model{..}
        = Frugel.prettyPrint -- pretty print twice, because program may not be fully parsed (and then it's only parsed but not pretty-printed)
        . Frugel.prettyPrint
        $ Frugel.initialModel p

-- Currently, the largest limiting factor on this is that rendering big partially evaluated programs
-- Otherwise it could be 20, which is still to low for real-world programs. To make partial evaluation useful for those, the editor could present iteratively further evaluated programs
initialFuelLimit :: Int
initialFuelLimit = 3

updateWithFrugelModel :: Frugel.Model Program -> Model -> Model
updateWithFrugelModel Frugel.Model{..}
                      Model{program = _, cursorOffset = _, errors = _, ..}
    = hideSelectedNodeValue
    $ Model { editableDataVersion = editableDataVersion + 1
            , program
            , cursorOffset
            , errors = map fromFrugelError errors
            , evaluationOutput = evaluationOutput { evaluated = program'
                                                        evaluationPlaceHolder
                                                        Nothing
                                                  }
            , ..
            }

-- Assumes program terminates
fromFrugelModel :: Model -> Frugel.Model Program -> IO Model
fromFrugelModel = partialFromFrugelModel Infinity

partialFromFrugelModel :: Limit -> Model -> Frugel.Model Program -> IO Model
partialFromFrugelModel fuel
                       scoutModel@Model{ editableDataVersion
                                       , focusedNodeValueIndex
                                       , selectedNodeValueRenderDepth
                                       }
                       Frugel.Model{..} = do
    (evaluated, (evalErrors, focusedNodeValues))
        <- runEval (Just cursorOffset) fuel evalProgram program
    pure
        $ Model { editableDataVersion = editableDataVersion + 1
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

updateWithFrugelErrors :: [Frugel.Error Program] -> Model -> Model
updateWithFrugelErrors newErrors
    = chain [ #editableDataVersion +~ 1, #errors %~ \oldErrors ->
    rights (map matchFrugelError oldErrors) ++ map fromFrugelError newErrors ]

toFrugelModel :: Model -> Frugel.Model Program
toFrugelModel Model{..}
    = Frugel.Model { errors = toListOf (folded % _FrugelError) errors, .. }

hideSelectedNodeValue :: Model -> Model
hideSelectedNodeValue model
    = model
    & #partiallyEvaluated .~ has #selectedNodeValue model
    & #selectedNodeValue .~ ExprNode evaluationPlaceHolder

evaluationPlaceHolder :: Expr
evaluationPlaceHolder = exprCstrSite' . toCstrSite . one $ Left "Evaluating..."