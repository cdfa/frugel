{-# LANGUAGE RecordWildCards #-}

module Scout.Model ( module Scout.Model, Model(Model) ) where

import Data.MultiSet  hiding ( map )

import qualified Frugel

import Optics.Extra

import Scout
import Scout.Internal.Model

-- Assumes program terminates
initialModel :: Program -> Model
initialModel
    = unsafeFromFrugelModel initialFuelLimit
    . Frugel.prettyPrint -- pretty print twice, because program may not be fully parsed (and then it's only parsed but not pretty-printed)
    . Frugel.prettyPrint
    . Frugel.initialModel

-- Currently, the largest limiting factor on this is that rendering big partially evaluated programs
-- Otherwise it could be 20, which is still to low for real-world programs. To make partial evaluation useful for those, the editor could present iteratively further evaluated programs
initialFuelLimit :: Int
initialFuelLimit = 10

-- Assumes program terminates
unsafeFromFrugelModel :: Int -> Frugel.Model Program -> Model
unsafeFromFrugelModel = partialFromFrugelModel Infinity

partialFromFrugelModel :: Limit -> Int -> Frugel.Model Program -> Model
partialFromFrugelModel fuel fuelLimit Frugel.Model{..}
    = Model { errors = map fromFrugelError errors
                  ++ map (uncurry $ flip EvaluationError)
                         (toOccurList evalErrors)
            , evalThreadId = Nothing
            , ..
            }
  where
    (evaluated, evalErrors) = runEval fuel $ evalProgram program

updateWithFrugelErrors :: [Frugel.Error Program] -> Model -> Model
updateWithFrugelErrors newErrors = over #errors $ \oldErrors ->
    rights (map matchFrugelError oldErrors) ++ map fromFrugelError newErrors

toFrugelModel :: Model -> Frugel.Model Program
toFrugelModel Model{..}
    = Frugel.Model { errors = toListOf (folded % _FrugelError) errors, .. }
