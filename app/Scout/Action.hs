module Scout.Action where

import Frugel ( GenericAction )

import Scout.Model

-- The Elm Architecture forces model changes to be centralised. Actions should describe the changes as precisely as possible given their origin
data Action
    = Init
    | GenerateRandom
    | Log String
    | PrettyPrint
    | GenericAction GenericAction
    | AsyncAction Int AsyncAction
    | FocusedNodeValueIndexAction FocusedNodeValueIndexAction
    | ChangeFuelLimit Int

data AsyncAction = EvaluationFinished Model | NewProgramGenerated Model

data FocusedNodeValueIndexAction = Increment | Decrement
