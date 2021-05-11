{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frugel.Decomposition
    ( module Frugel.Internal.DecompositionState
    , CstrSiteZipper
    , Decomposable(..)
    , modifyNodeAt
    ) where

import           Control.Monad.Except
import           Control.Zipper.Seq

import           Data.Has
import           Data.Text.Optics

import           Frugel.Error
import           Frugel.Internal.DecompositionState
import           Frugel.Internal.Meta
                 ( Meta(interstitialWhitespace) )
import           Frugel.Internal.Node
                 ( Decl(meta, name, value), _Identifier )
import           Frugel.Internal.Program            as Program
                 ( Program(meta, expr, whereClause) )
import           Frugel.Node
import           Frugel.Program

import           Numeric.Optics

import           Optics

type CstrSiteZipper = SeqZipper (Either Char Node)

class Decomposable n where
    decomposed :: n -> CstrSite
    conservativelyDecompose :: Int -> n -> Maybe (Int, CstrSite)
    conservativelyDecompose _ _ = Nothing
    -- It would make sense for this function to have a mapKeyword :: Text -> f () and mapWhitespace :: Char -> f Char as well, but it's not yet needed
    -- With writing more boilerplate, it would also be possible to generalize this for applicative functors instead of monads
    -- Except for the Monad constraint, this function is like a monomorphic Bitraversable instance
    mapMComponents :: Monad m
        => (Char -> m Char)
        -> (forall n'. (Decomposable n', CstrSiteNode n') => n' -> m n')
        -> n
        -> m n

class CstrSiteNode n where
    fromCstrSite :: CstrSite -> n

instance CstrSiteNode Identifier where
    fromCstrSite = IdentifierCstrSite

instance CstrSiteNode Expr where
    fromCstrSite = ExprCstrSite defaultExprMeta

instance CstrSiteNode Decl where
    fromCstrSite = DeclCstrSite defaultMeta

instance CstrSiteNode WhereClause where
    fromCstrSite = WhereCstrSite defaultMeta

instance CstrSiteNode Program where
    fromCstrSite = ProgramCstrSite defaultProgramMeta

step :: MonadState DecompositionState m => m ()
step = do
    textOffset <- use #textOffset
    -- c <- guse #cstrSiteOffset
    -- traceM ("step t: " <> show textOffset <> " c: " <> show c)
    when (textOffset /= -1) (#textOffset -= 1)
    when (textOffset > 0) (#cstrSiteOffset += 1)

modifyNodeAt :: forall m'.
    MonadError InternalError m'
    => (Int -> CstrSite -> m' CstrSite)
    -> Int
    -> Program
    -> m' Program
modifyNodeAt f cursorOffset program
    = runModification >>= \(newProgram, decompositionState) -> if
        | view #textOffset decompositionState
            > 0 -> throwError $ DecompositionFailed cursorOffset
        | Todo <- view #modificationStatus decompositionState ->
            throwError $ ASTModificationNotPerformed cursorOffset
        | otherwise -> pure newProgram
  where
    runModification
        = mapNode program `runStateT` initialDecompositionState cursorOffset
    mapChar c = c <$ step -- trace (show c) step
    mapNode :: (Decomposable n, CstrSiteNode n) => n -> DecompositionMonad m' n
    mapNode n = do
        -- t <- guse #textOffset
        -- traceM ("pre " <> take 20 (show n) <> " " <> show t)
        ifM (guses #textOffset (< 0)) (pure n) -- node is located after cursor
            $ do
                #cstrSiteOffset += 1 -- At the moment, the construction site offset passed to `f` is immediately after a node where applying `f` failed. This is not a good default.
                withLocal #cstrSiteOffset 0 $ do
                    newNode <- mapMComponents mapChar mapNode n
                    -- t <- guse #textOffset
                    -- c <- guse #cstrSiteOffset
                    -- traceM
                    --     ("post "
                    --      <> take 20 (show n)
                    --      <> " t: "
                    --      <> show t
                    --      <> " c: "
                    --      <> show c)
                    ifM
                        (guses #textOffset (<= 0)
                         &&^ guses #modificationStatus (isn't _Success))
                        (catchError
                             (fromCstrSite <$> transform n
                              <* assign #modificationStatus Success)
                         $ const (pure n))
                        (pure newNode)
    -- transform :: (Decomposable n) => n -> StateT DecompositionState m' CstrSite
    transform n = do
        cstrSiteOffset <- use #cstrSiteOffset
        lift $ case conservativelyDecompose cstrSiteOffset n of
            Just (cstrSiteOffset', cstrSite)
                | cstrSiteOffset == 0
                    || cstrSiteOffset == length (toList $ decomposed n) ->
                    catchError (f cstrSiteOffset' cstrSite)
                    $ const (f cstrSiteOffset $ decomposed n)
            _ -> f cstrSiteOffset $ decomposed n

intersperseWhitespace' :: [Text] -> CstrSite' -> CstrSite
intersperseWhitespace' whitespaceFragments
    = fromList
    . intersperseWhitespace (map Left . toString) whitespaceFragments
    . map (either (map Left) (one . Right))

intersperseWhitespaceTraversals :: (Monad m, Has Meta n)
    => (Char -> m Char)
    -> n
    -> [n
       -> m n]
    -> [n
       -> m n]
intersperseWhitespaceTraversals mapChar n traversals
    = interleave
        [ traversals
        , imap (\i _ -> whitespaceFragmentTraversal (ix i) %%~ mapChar)
          $ view (hasLens @Meta % #interstitialWhitespace) n
        ]

whitespaceFragmentTraversal :: (Is k An_AffineTraversal, Has Meta n)
    => Optic' k NoIx [Text] Text
    -> Traversal' n Char
whitespaceFragmentTraversal selector
    = hasLens @Meta
    % #interstitialWhitespace
    % castOptic @An_AffineTraversal selector
    % unpacked
    % traversed

conservativelyDecomposeNode
    :: (Is k A_Review, Is l An_AffineFold, Decomposable n)
    => Optic' k is Node n
    -> Optic' l is n CstrSite
    -> Int
    -> n
    -> Maybe (Int, CstrSite)
conservativelyDecomposeNode nodeReview cstrSiteFold cstrSiteOffset n
    = case cstrSiteOffset of
        0
            | isn't cstrSiteFold n -> Just (0, singletonCstrSite)
        l
            | isn't cstrSiteFold n && l == length (toList $ decomposed n) ->
                Just (1, singletonCstrSite)
        _ -> Nothing
  where
    singletonCstrSite = fromList [ Right $ review nodeReview n ]

instance Decomposable CstrSite where
    decomposed = id
    mapMComponents mapChar mapNode
        = traverseOf (_CstrSite % traversed) . bitraverse mapChar
        $ mapMComponents mapChar mapNode

instance Decomposable Node where
    decomposed (IdentifierNode n) = decomposed n
    decomposed (ExprNode n) = decomposed n
    decomposed (DeclNode n) = decomposed n
    decomposed (WhereNode n) = decomposed n
    mapMComponents _ mapNode n = case n of
        IdentifierNode identifier -> IdentifierNode <$> mapNode identifier
        ExprNode expr -> ExprNode <$> mapNode expr
        DeclNode decl -> DeclNode <$> mapNode decl
        WhereNode whereClause -> WhereNode <$> mapNode whereClause

instance Decomposable Identifier where
    decomposed (Identifier name)
        = CstrSite . fromList . map Left $ toString name
    decomposed (IdentifierCstrSite materials) = materials
    conservativelyDecompose
        = conservativelyDecomposeNode _IdentifierNode _IdentifierCstrSite
    mapMComponents mapChar _ identifier@(Identifier _)
        = traverseOf (_Identifier % unpacked % traversed) mapChar identifier
    mapMComponents mapChar mapNode (IdentifierCstrSite materials)
        = IdentifierCstrSite <$> mapMComponents mapChar mapNode materials

instance Decomposable Expr where
    decomposed e
        = either
            (intersperseWhitespace'
                 (e ^. exprMeta % #standardMeta % #interstitialWhitespace)
             . decomposed')
            parenthesize
        $ unwrapParentheses e
      where
        parenthesize (leadingFragment, e2, trailingFragment)
            = toCstrSite
                [ Left "("
                , whitespaceFragment leadingFragment
                , Right $ ExprNode e2
                , whitespaceFragment trailingFragment
                , Left ")"
                ]
        whitespaceFragment = Left . toString
        decomposed' (Variable _ name) = [ Right $ IdentifierNode name ]
        decomposed' (Abstraction _ name body)
            = [ Left [ '\\' ]
              , Right $ IdentifierNode name
              , Left "="
              , Right $ ExprNode body
              ]
        decomposed' (Application _ function arg)
            = [ Right $ ExprNode function, Right $ ExprNode arg ]
        decomposed' (Sum _ left right)
            = [ Right $ ExprNode left, Left "+", Right $ ExprNode right ]
        decomposed' (ExprCstrSite _ (CstrSite materials))
            = map (first one) $ toList materials
    conservativelyDecompose
        = conservativelyDecomposeNode _ExprNode (_ExprCstrSite % _2)
    mapMComponents mapChar mapNode e = e & case e of
        _
            | e ^. exprMeta % #parenthesisLevels > 0 -> chain
                [ (<$ mapChar '(')
                , whitespaceFragmentTraversal _head %%~ mapChar
                , refracting (exprMeta % #parenthesisLevels) (subtracting 1)
                  % refracting
                      (exprMeta % #standardMeta % #interstitialWhitespace)
                      (_tail % _init)
                  %%~ mapNode
                , whitespaceFragmentTraversal _last %%~ mapChar
                , (<$ mapChar ')')
                ]
        -- All these cases could be composed into 1, because the lenses don't overlap, but this is better for totality checking
        Variable _ _ -> _Variable % _2 %%~ mapNode
        Abstraction{} -> chain
            $ intersperseWhitespaceTraversals'
                [ (<$ mapChar '\\')
                , _Abstraction % _2 %%~ mapNode
                , (<$ mapChar '=')
                , _Abstraction % _3 %%~ mapNode
                ]
        Application{} -> chain
            $ intersperseWhitespaceTraversals'
                [ _Application % _2 %%~ mapNode
                , _Application % _3 %%~ mapNode
                ]
        Sum{} -> chain
            $ intersperseWhitespaceTraversals'
                [ _Sum % _2 %%~ mapNode
                , (<$ mapChar '+')
                , _Sum % _3 %%~ mapNode
                ]
        ExprCstrSite{} -> _ExprCstrSite % _2 %%~ mapMComponents mapChar mapNode
      where
        intersperseWhitespaceTraversals'
            = intersperseWhitespaceTraversals mapChar e

instance Decomposable Decl where
    decomposed Decl{..}
        = intersperseWhitespace'
            (interstitialWhitespace meta)
            [ Right $ IdentifierNode name, Left "=", Right $ ExprNode value ]
    decomposed (DeclCstrSite _ materials) = materials
    conservativelyDecompose
        = conservativelyDecomposeNode _DeclNode (_DeclCstrSite % _2)
    mapMComponents mapChar mapNode decl@Decl{}
        = chain
            (intersperseWhitespaceTraversals
                 mapChar
                 decl
                 [ traverseOf #name mapNode
                 , (<$ mapChar '=')
                 , traverseOf #value mapNode
                 ])
            decl
    mapMComponents mapChar mapNode (DeclCstrSite meta materials)
        = DeclCstrSite meta <$> mapMComponents mapChar mapNode materials

instance Decomposable WhereClause where
    decomposed (WhereClause meta decls)
        = intersperseWhitespace'
            (interstitialWhitespace meta)
            (Left "where" : map (Right . DeclNode) (toList decls))
    decomposed (WhereCstrSite _ materials) = materials
    conservativelyDecompose
        = conservativelyDecomposeNode _WhereNode (_WhereCstrSite % _2)
    mapMComponents mapChar mapNode whereClause@(WhereClause _ decls)
        = chain
            (intersperseWhitespaceTraversals
                 mapChar
                 whereClause
                 ((<$ traverse_ @[] mapChar "where")
                  : imap
                      (\i _ -> _WhereClause % _2 % ix i %%~ mapNode)
                      (toList decls)))
            whereClause
    mapMComponents mapChar mapNode (WhereCstrSite meta materials)
        = WhereCstrSite meta <$> mapMComponents mapChar mapNode materials

instance Decomposable Program where
    decomposed Program{..}
        = intersperseWhitespace'
            (meta ^. #standardMeta % #interstitialWhitespace)
            (Right (ExprNode expr)
             : (Right . WhereNode <$> maybeToList whereClause)
             ++ [ Left . toString $ view #trailingWhitespace meta ])
    decomposed (ProgramCstrSite _ materials) = materials
    mapMComponents mapChar mapNode program@Program{}
        = chain
            (intersperseWhitespaceTraversals
                 mapChar
                 program
                 [ traverseOf #expr mapNode, #whereClause % _Just %%~ mapNode ]
             :> (#meta % #trailingWhitespace % unpacked % traversed
                 %%~ mapChar))
            program
    mapMComponents mapChar mapNode (ProgramCstrSite meta materials)
        = ProgramCstrSite meta <$> mapMComponents mapChar mapNode materials
