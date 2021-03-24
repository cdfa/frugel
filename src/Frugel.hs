{-# LANGUAGE FlexibleContexts #-}

module Frugel
    ( module Frugel
    , prettyProgram
    , prettyCstrMaterials
    , parseErrorPretty
    ) where

import           Miso             hiding ( node )
import           Text.Megaparsec
import           Node
import           Internal.Program ( Program, prettyProgram )
import           Parsing

-- Type synonym for an application model
type Model = CstrMaterials

initialModel :: CstrMaterials
initialModel = Node.whereClauseTest

-- Sum type for application events
-- data Action = AddOne | SubtractOne | NoOp | SayHelloWorld
data Action = NoOp
    deriving ( Show, Eq )

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

-- updateModel AddOne m = noEff (m + 1)
-- updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp = noEff

-- updateModel SayHelloWorld m =
--     m <# do liftIO (putStrLn "Hello World") >> pure NoOp
parseCstrSite :: FilePath
    -> CstrMaterials
    -> Either (NonEmpty (ParseError CstrMaterials Void)) Program
parseCstrSite filePath cstrMaterials
    = first bundleErrors $ runParser (program <* eof) filePath cstrMaterials
