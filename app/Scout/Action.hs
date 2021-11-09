module Scout.Action where

import Frugel ( GenericAction )
import qualified Frugel

import Scout
import Scout.Model

-- The Elm Architecture forces model changes to be centralised. Actions should describe the changes as precisely as possible given their origin
data Action
    = Init
    | GenerateRandom
    | Log String
    | PrettyPrint
    | GenericAction GenericAction
    | AsyncAction AsyncAction
    | FocusedNodeValueIndexAction FocusedNodeValueIndexAction
    | ChangeFuelLimit Int
    | ChangeSelectedNodeValueRenderDepth Int

data AsyncAction
    = EvaluationFinished Model | NewProgramGenerated (Frugel.Model Program)

data FocusedNodeValueIndexAction = Increment | Decrement
