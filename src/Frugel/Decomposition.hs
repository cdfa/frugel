{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frugel.Decomposition
    ( module Frugel.Decomposition
    , module Frugel.Internal.DecompositionState
    ) where

import           Data.Has
import           Data.Text.Optics

import           Frugel.Internal.DecompositionState
import           Frugel.Node
import           Frugel.Parsing                     ( Parser, anyNode )
import           Frugel.Program

import           Numeric.Optics

import           Optics

class Decomposable n where
    conservativelyDecompose :: Int -> n -> Maybe (Int, CstrSite)
    conservativelyDecompose _ _ = Nothing
    hasParser :: Maybe (Parser n)
    hasParser = Nothing
    -- It would make sense for this function to have a mapKeyword :: Text -> f () and mapWhitespace :: Char -> f Char as well, but it's not yet needed
    -- With writing more boilerplate, it would also be possible to generalize this for applicative functors instead of monads
    -- Except for the Monad constraint, this function is like a monomorphic Bitraversable instance
    mapMComponents :: Monad m
        => (Char -> m Char)
        -> (forall n'. (Decomposable n', IsNode n') => n' -> m n')
        -> n
        -> m n

decompose :: Decomposable n => n -> CstrSite
decompose n
    = fromList . reverse
    $ execState
        (mapMComponents (consing Left) (consing (Right . review nodePrism)) n)
        []
  where
    consing f x = x <$ modify (f x :)

textLength :: Decomposable n => n -> Int
textLength
    = sum . fmap (either (const 1) textLength) . view _CstrSite . decompose

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
            | isn't cstrSiteFold n && l == length (toList $ decompose n) ->
                Just (1, singletonCstrSite)
        _ -> Nothing
  where
    singletonCstrSite = fromList [ Right $ review nodeReview n ]

instance Decomposable CstrSite where
    mapMComponents mapChar mapNode
        = traverseOf (_CstrSite % traversed) $ bitraverse mapChar mapNode

instance Decomposable Node where
    hasParser = Just anyNode
    conservativelyDecompose cstrSiteOffset = \case
        ExprNode expr -> conservativelyDecompose cstrSiteOffset expr
        DeclNode decl -> conservativelyDecompose cstrSiteOffset decl
        WhereNode
            whereClause -> conservativelyDecompose cstrSiteOffset whereClause
    mapMComponents mapChar mapNode = \case
        ExprNode expr -> ExprNode <$> mapMComponents mapChar mapNode expr
        DeclNode decl -> DeclNode <$> mapMComponents mapChar mapNode decl
        WhereNode whereClause ->
            WhereNode <$> mapMComponents mapChar mapNode whereClause

instance Decomposable Identifier where
    mapMComponents mapChar _ identifier@(Identifier _)
        = traverseOf
            (_Identifier % traversed % #unAlphanumeric)
            (\c -> c <$ mapChar c)
            identifier

instance Decomposable Expr where
    conservativelyDecompose
        = conservativelyDecomposeNode _ExprNode (_ExprCstrSite % _2)
    mapMComponents mapChar mapNode e
        | e ^. exprMeta % #parenthesisLevels > 0
            = chain
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
                e
    mapMComponents mapChar mapNode e = e & case e of
        -- All these cases could be composed into 1, because the lenses don't overlap, but this is better for totality checking
        Variable{} -> _Variable % _2 %%~ mapMComponents mapChar mapNode
        Abstraction{} -> chain
            $ intersperseWhitespaceTraversals'
                [ (<$ mapChar '\\')
                , _Abstraction % _2 %%~ mapMComponents mapChar mapNode
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
    conservativelyDecompose
        = conservativelyDecomposeNode _DeclNode (_DeclCstrSite % _2)
    mapMComponents mapChar mapNode decl@Decl{}
        = chain
            (intersperseWhitespaceTraversals
                 mapChar
                 decl
                 [ traverseOf #name $ mapMComponents mapChar mapNode
                 , (<$ mapChar '=')
                 , traverseOf #value mapNode
                 ])
            decl
    mapMComponents mapChar mapNode (DeclCstrSite meta materials)
        = DeclCstrSite meta <$> mapMComponents mapChar mapNode materials

instance Decomposable WhereClause where
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
