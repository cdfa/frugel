module Frugel.Error where

import Frugel.Node

import Text.Megaparsec.Error hiding ( parseErrorPretty )

data Error
    = ParseError (ParseError CstrSite Void) | InternalError InternalError
    deriving ( Show, Eq )

data InternalError
    = ParseFailedAfterPrettyPrint
    | ASTModificationNotPerformed Int
    | DecompositionFailed Int
    | CstrSiteActionFailed Int CstrSite
    deriving ( Show, Eq )
