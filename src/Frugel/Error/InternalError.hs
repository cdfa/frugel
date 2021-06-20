module Frugel.Error.InternalError where

import Frugel.CstrSite

data InternalError p
    = ParseFailedAfterPrettyPrint
    | ASTModificationNotPerformed Int
    | DecompositionFailed Int
    | CstrSiteActionFailed Int (ACstrSite (NodeOf p))