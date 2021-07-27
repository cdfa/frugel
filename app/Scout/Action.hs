module Scout.Action where

import Frugel ( GenericAction )

import Scout.Model

data Action
    = Init
    | GenerateRandom
    | ModifyModel (Model -> Model)
    | Log String
    | PrettyPrint
    | GenericAction GenericAction
