module Scout.Action where

import Frugel ( GenericAction )

import Scout.Model

-- The Elm Architecture forces model changes to be centralised. Actions should describe the changes as precisely as possible given their origin
data Action
    = Init
    | GenerateRandom
    | ModifyModel (Model -> Model)
    | Log String
    | PrettyPrint
    | GenericAction GenericAction
    | FocusedNodeValueIndexAction FocusedNodeValueIndexAction

data FocusedNodeValueIndexAction = Increment | Decrement
