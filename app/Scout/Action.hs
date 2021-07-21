module Scout.Action where

import Frugel

import Scout

data Action
    = Init
    | GenerateRandom
    | NewModel (Model Program)
    | Log String
    | PrettyPrint
    | GenericAction GenericAction
