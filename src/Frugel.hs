module Frugel where

import           Miso

-- Type synonym for an application model
type Model = Int

-- Sum type for application events
data Action = AddOne | SubtractOne | NoOp | SayHelloWorld
    deriving ( Show, Eq )

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m =
    m <# do liftIO (putStrLn "Hello World") >> pure NoOp
