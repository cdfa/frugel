module Frugel.Error.InternalError where

import Frugel.CstrSite

data InternalError p
    = ASTModificationNotPerformed Int
    | DecompositionFailed Int
    | CstrSiteActionFailed Int (ACstrSite (NodeOf p))