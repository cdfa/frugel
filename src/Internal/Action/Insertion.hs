{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Action.Insertion where

import           Node
import           Optics

data InsertionState = InsertionState { cursorOffset :: Integer, char :: Char }

makeFieldLabelsWith noPrefixFieldLabels ''InsertionState

class Decomposable n where
    insertByPos :: MonadState (Maybe InsertionState) m => n -> m CstrMaterials

instance Decomposable CstrMaterials where
    insertByPos = traverseOf _CstrMaterials $ foldlM foldMaterials empty
      where
        foldMaterials :: MonadState (Maybe InsertionState) m
            => Seq (Either Char Node)
            -> Either Char Node
            -> m (Seq (Either Char Node))
        foldMaterials materials item
            = maybe
                (pure (materials |> item))
                (\InsertionState{..} -> if cursorOffset == 0 then do
                     put Nothing
                     pure (materials |> Left char |> item) else do
                     _Just % #cursorOffset -= 1
                     itemMaterials
                         <- either (pure . one . Left) processNodeItem item
                     pure (materials >< itemMaterials))
            =<< get
        processNodeItem node = do
            nodeMaterials <- insertByPos node
            gets
                (bool (view _CstrMaterials nodeMaterials) (one $ Right node)
                 . any ((> 0) . view #cursorOffset))

instance Decomposable Node where
    insertByPos (ExprNode (Identifier meta name)) = undefined

intersperseWhitespace :: [Text] -> CstrMaterials -> CstrMaterials
intersperseWhitespace whitespaceFragments decomposables
    = fromList . concat
    $ interleave
        [ map one . toList $ view _CstrMaterials decomposables
        , map (map Left . toString) whitespaceFragments
        ]
-- intersperseWhitespace :: MonadState (Maybe InsertionState) m
--     => [Text]
--     -> [m CstrMaterials]
--     -> m CstrMaterials
-- intersperseWhitespace whitespaceFragments decomposables
--     = concatCstrMaterials
--     <$> sequence
--         (interleave
--              [ decomposables
--              , map
--                    (insertByPos
--                     . CstrMaterials
--                     . fromList
--                     . map Left
--                     . toString)
--                    whitespaceFragments
--              ])
