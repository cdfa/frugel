{-# LANGUAGE FlexibleContexts #-}

module Action where

import           Prelude                   hiding ( one )
import           Model
import           Internal.Meta             ( defaultProgramMeta )
import           Internal.Program          ( Program(ProgramCstrSite) )
import           Parsing
import           Internal.Action.Insertion
import           Text.Megaparsec.Pos

update :: Model -> Char -> Model
update model c
    = model { Model.program = fromRight
                  (ProgramCstrSite defaultProgramMeta inserted)
                  $ parseCstrSite fileName inserted
            }
  where
    inserted
        = evalState
            (insertByPos $ Model.program model)
            (Just
                 InsertionState { cursorOffset
                                      = offSetBySourcePos $ cursorPos model
                                , char         = c
                                })

offSetBySourcePos :: SourcePos -> Integer
offSetBySourcePos = error "not implemented"
-- insertByPos c pos program =    case program model of
--         Program{..} -> _
--         ProgramCstrSite meta materials -> CstrMaterials meta
--             $ insertByPos nodeUpdate (cursorPos model) materials
--   where
--     absoluteCursorPos = cursorPos model
--     nodeUpdate cursorPos
--         = \case
--             ExprNode expr -> exprUpdate cursorPos expr
--             DeclNode decl -> declUpdate cursorPos decl
--             WhereNode whereClause -> whereUpdate cursorPos whereClause
--     exprUpdate cursorPos@SourcePos{..}
--         = \case
--             Identifier meta name -> textUpdate (column - 2) name
--             Abstraction meta name body -> if body `containsPos` cursorPos
--                 then Left '\\'
--                     : (map Left name
--                        ++ one (Left '=')
--                        ++ exprUpdate updatedCursorPos body)
--                 else Left '\\'
--                     : (textUpdate nameIndex name
--                        ++ fromList [ Left '=', Right body ])
--               where
--                 updatedBodyCursorPos = _
--                 nameIndex = _
--             Application meta Expr Expr -> _
--             Sum meta Expr Expr -> _
--             ExprCstrSite meta CstrMaterials -> _
--     textUpdate i = map Left $ insertAt i c
-- one :: _
-- one = _
-- insertByPos
--     :: (SourcePos -> Node -> CstrMaterials) -> SourcePos -> CstrMaterials -> CstrMaterials
-- insertByPos = _
