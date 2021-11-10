{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Scout.Model ( module Scout.Model, Model(Model) ) where

import qualified Data.MultiSet as MultiSet

import qualified Frugel

import Optics.Extra.Scout

import Scout
import Scout.Internal.Model

initialModel :: Program -> Model
initialModel p
    = Model { editableDataVersion = 0
            , fuelLimit = initialFuelLimit
            , selectedNodeEvaluationIndex = 0
            , errors = []
            , partiallyEvaluated = False
            , selectedNodeValueRenderDepth = 10
            , contextRenderDepth = 5
            , definitionsViewCollapsed = True
            , evaluationOutput
                  = EvaluationOutput { evaluated = program'
                                           evaluationPlaceHolder
                                           Nothing
                                     , focusedNodeEvaluations = mempty
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
                       Model{program = _, cursorOffset = _, errors = _, ..}
                       Frugel.Model{..} = do
    (evaluated, (evalErrors, focusedNodeEvaluations))
        <- runEval (Just cursorOffset) fuel evalProgram program
    pure
        $ Model { editableDataVersion = editableDataVersion + 1
                , errors = map fromFrugelError errors
                      ++ map (uncurry $ flip EvaluationError)
                             (MultiSet.toOccurList evalErrors)
                , partiallyEvaluated = case fuel of
                      Only _ -> True
                      Infinity -> False
                , evaluationOutput = EvaluationOutput { .. }
                , ..
                }

updateWithFrugelErrors :: [Frugel.Error Program] -> Model -> Model
updateWithFrugelErrors newErrors
    = chain [ #editableDataVersion +~ 1, #errors %~ \oldErrors ->
    rights (map matchFrugelError oldErrors) ++ map fromFrugelError newErrors ]

toFrugelModel :: Model -> Frugel.Model Program
toFrugelModel Model{..}
    = Frugel.Model { errors = toListOf (folded % _FrugelError) errors, .. }

contextInView :: Model -> Bool
contextInView model = definitionsInView model || variablesInView model

definitionsInView :: Model -> Bool
definitionsInView model
    = not (view #definitionsViewCollapsed model)
    && has (#selectedNodeEvaluation % #definitions % folded) model

variablesInView :: Model -> Bool
variablesInView = has $ #selectedNodeEvaluation % #variables % folded

-- force errors to force full evaluation
forceMainExpression :: Model -> Model
forceMainExpression model@Model{..} = seq (length errors) model

forceSelectedNodeValue :: Model -> Model
forceSelectedNodeValue = forceSelectedNodeField Value #value

forceSelectedNodeContext :: Model -> Model
forceSelectedNodeContext model
    = applyWhen (not $ view #definitionsViewCollapsed model)
                forceSelectedNodeDefinitions
    $ forceSelectedNodeVariables model

forceSelectedNodeDefinitions :: Model -> Model
forceSelectedNodeDefinitions
    = forceSelectedNodeField Context $ #definitions % folded % to ExprNode

forceSelectedNodeVariables :: Model -> Model
forceSelectedNodeVariables
    = forceSelectedNodeField Context $ #variables % folded % to ExprNode

forceSelectedNodeField :: Is k A_Fold
    => RenderDepthField
    -> Optic' k is FocusedNodeEvaluation Node
    -> Model
    -> Model
forceSelectedNodeField field fieldNodes model
    = seq (lengthOf (#selectedNodeEvaluation
                     % castOptic @A_Fold fieldNodes
                     % to (capTree $ view (renderDepthFieldLens field) model)
                     % allEvaluatedChildren)
                    model)
          model

hideSelectedNodeEvaluation :: Model -> Model
hideSelectedNodeEvaluation
    = hideSelectedNodeValue . hideSelectedNodeDefinitions

hideSelectedNodeValue :: Model -> Model
hideSelectedNodeValue = hideSelectedEvaluationField $ #value % _ExprNode

hideSelectedNodeContext :: Model -> Model
hideSelectedNodeContext
    = hideSelectedNodeDefinitions . hideSelectedNodeVariables

hideSelectedNodeDefinitions :: Model -> Model
hideSelectedNodeDefinitions
    = hideSelectedEvaluationField $ #definitions % traversed

hideSelectedNodeVariables :: Model -> Model
hideSelectedNodeVariables
    = hideSelectedEvaluationField $ #variables % traversed

hideSelectedEvaluationField :: Is k A_Traversal
    => Optic' k is FocusedNodeEvaluation Expr
    -> Model
    -> Model
hideSelectedEvaluationField (castOptic @A_Traversal -> fieldNodes) model
    = model
    & #partiallyEvaluated .~ has (#selectedNodeEvaluation % fieldNodes) model
    & #selectedNodeEvaluation % fieldNodes .~ evaluationPlaceHolder

evaluationPlaceHolder :: Expr
evaluationPlaceHolder = exprCstrSite' . toCstrSite . one $ Left "Evaluating..."

data RenderDepthField = Value | Context
    deriving ( Show, Eq )

renderDepthFieldLens :: RenderDepthField -> Lens' Model Int
renderDepthFieldLens Value = #selectedNodeValueRenderDepth
renderDepthFieldLens Context = #contextRenderDepth

forceFieldValues :: RenderDepthField -> Model -> Model
forceFieldValues Value = forceSelectedNodeValue
forceFieldValues Context = forceSelectedNodeContext

hideFieldValues :: RenderDepthField -> Model -> Model
hideFieldValues Value = hideSelectedNodeValue
hideFieldValues Context = hideSelectedNodeContext
