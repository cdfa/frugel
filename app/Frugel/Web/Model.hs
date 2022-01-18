{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Frugel.Web.Model
    ( module Frugel.Web.Model
    , Model(Model)
    , EvaluationStatus(..)
    , EvaluationOutput(EvaluationOutput)
    ) where

import qualified Data.MultiSet as MultiSet

import qualified Frugel
import Frugel.Web.Internal.Model

import Optics.Extra.Scout

import Scout               hiding ( Evaluated, EvaluationStatus )

initialModel :: Program -> Model
initialModel p
    = Model { editableDataVersion = 0
            , fuelLimit = 20
            , limitEvaluationByDefault = False
            , selectedNodeEvaluationIndex = 0
            , errors = []
            , showHelp = True
            , evaluationStatus = PartiallyEvaluated
            , mainExpressionRenderDepth = 20
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

updateWithFrugelModel :: Frugel.Model Program -> Model -> Model
updateWithFrugelModel Frugel.Model{..}
                      Model{program = _, cursorOffset = _, errors = _, ..}
    = hideEvaluationOutput
    $ Model { editableDataVersion = editableDataVersion + 1
            , program
            , cursorOffset
            , errors = map fromFrugelError errors
            , ..
            }

-- Assumes program terminates
fromFrugelModel :: Model -> Frugel.Model Program -> IO Model
fromFrugelModel = partialFromFrugelModel Infinity

partialFromFrugelModel :: Limit -> Model -> Frugel.Model Program -> IO Model
partialFromFrugelModel fuel
                       Model{program = _, cursorOffset = _, errors = _, ..}
                       Frugel.Model{..} = do
    (evaluated, (evalErrors, focusedNodeEvaluations)) <- runEval
        (Just cursorOffset)
        limitEvaluationByDefault
        fuel
        evalProgram
        program
    pure
        $ Model { editableDataVersion = editableDataVersion + 1
                , errors = map fromFrugelError errors
                      ++ map (uncurry $ flip EvaluationError)
                             (MultiSet.toOccurList evalErrors)
                , evaluationStatus = if limitEvaluationByDefault
                      then Evaluated
                      else case fuel of
                          Only _ -> PartiallyEvaluated
                          Infinity -> Evaluated
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
forceSelectedNodeValue = forceSelectedNodeField SelectedNodeValue #value

forceSelectedNodeContext :: Model -> Model
forceSelectedNodeContext model
    = applyWhen (not $ view #definitionsViewCollapsed model)
                forceSelectedNodeDefinitions
    $ forceSelectedNodeVariables model

forceSelectedNodeDefinitions :: Model -> Model
forceSelectedNodeDefinitions
    = forceSelectedNodeField SelectedNodeContext
    $ #definitions % folded % to ExprNode

forceSelectedNodeVariables :: Model -> Model
forceSelectedNodeVariables
    = forceSelectedNodeField SelectedNodeContext
    $ #variables % folded % to ExprNode

forceSelectedNodeField :: Is k A_Fold
    => RenderDepthField
    -> Optic' k is FocusedNodeEvaluation Node
    -> Model
    -> Model
forceSelectedNodeField field fieldNodes model
    = seq (lengthOf (#selectedNodeEvaluation
                     % castOptic @A_Fold fieldNodes
                     % to (truncate $ view (renderDepthFieldLens field) model)
                     % allEvaluatedChildren)
                    model)
          model

hideEvaluationOutput :: Model -> Model
hideEvaluationOutput = hideMainExpression . hideSelectedNodeEvaluation

hideMainExpression :: Model -> Model
hideMainExpression
    = hideEvaluationOutputField $ #evaluationOutput % #evaluated % #expr

hideSelectedNodeEvaluation :: Model -> Model
hideSelectedNodeEvaluation
    = hideSelectedNodeValue . hideSelectedNodeDefinitions

hideSelectedNodeValue :: Model -> Model
hideSelectedNodeValue
    = hideEvaluationOutputField $ #selectedNodeEvaluation % #value % _ExprNode

hideSelectedNodeContext :: Model -> Model
hideSelectedNodeContext
    = hideSelectedNodeDefinitions . hideSelectedNodeVariables

hideSelectedNodeDefinitions :: Model -> Model
hideSelectedNodeDefinitions
    = hideEvaluationOutputField
    $ #selectedNodeEvaluation % #definitions % traversed

hideSelectedNodeVariables :: Model -> Model
hideSelectedNodeVariables
    = hideEvaluationOutputField
    $ #selectedNodeEvaluation % #variables % traversed

hideEvaluationOutputField
    :: Is k A_Traversal => Optic' k is Model Expr -> Model -> Model
hideEvaluationOutputField (castOptic @A_Traversal -> fieldNodes) model
    = model
    & fieldNodes .~ evaluationPlaceHolder
    & #evaluationStatus
    .~ if has fieldNodes model then PartiallyEvaluated else Evaluated

evaluationPlaceHolder :: Expr
evaluationPlaceHolder = exprCstrSite' . toCstrSite . one $ Left "Evaluating..."

data RenderDepthField
    = MainExpression | SelectedNodeValue | SelectedNodeContext
    deriving ( Show, Eq )

renderDepthFieldLens :: RenderDepthField -> Lens' Model Int
renderDepthFieldLens MainExpression = #mainExpressionRenderDepth
renderDepthFieldLens SelectedNodeValue = #selectedNodeValueRenderDepth
renderDepthFieldLens SelectedNodeContext = #contextRenderDepth

forceFieldValues :: RenderDepthField -> Model -> Model
forceFieldValues MainExpression = forceMainExpression
forceFieldValues SelectedNodeValue = forceSelectedNodeValue
forceFieldValues SelectedNodeContext = forceSelectedNodeContext

hideFieldValues :: RenderDepthField -> Model -> Model
hideFieldValues MainExpression = hideMainExpression
hideFieldValues SelectedNodeValue = hideSelectedNodeValue
hideFieldValues SelectedNodeContext = hideSelectedNodeContext
