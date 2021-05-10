{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE MultiWayIf #-}

{-# LANGUAGE NamedFieldPuns #-}
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

import           Frugel.Internal.DecompositionState
import           Frugel.Internal.Meta
                 ( Meta(interstitialWhitespace) )
import           Frugel.Internal.Node
                 ( Decl(meta, name, value), _Identifier )
import           Frugel.Internal.Program            as Program
                 ( Program(meta, expr, whereClause) )
import           Frugel.Node
import           Frugel.PrettyPrinting
import           Frugel.Program

import           Numeric.Optics

import           Optics

type CstrSiteZipper = SeqZipper (Either Char Node)

class Decomposable n where
    decomposed :: n -> CstrSite
    -- It would make sense for this function to have a mapKeyword :: Text -> f () and mapWhitespace :: Char -> f Char as well, but it's not yet needed
    -- With writing more boilerplate, it would also be possible to generalize this for applicative functors instead of monads
    -- Except for the Monad constraint, this function is like a monomorphic Bitraversable instance
    mapMComponents :: Monad m => n -> DecompositionMonad m n

class CstrSiteNode n where
    cstrSiteConstructor :: CstrSite -> n

instance CstrSiteNode Identifier where
    cstrSiteConstructor = IdentifierCstrSite

instance CstrSiteNode Expr where
    cstrSiteConstructor = ExprCstrSite defaultExprMeta

instance CstrSiteNode Decl where
    cstrSiteConstructor = DeclCstrSite defaultMeta

instance CstrSiteNode WhereClause where
    cstrSiteConstructor = WhereCstrSite defaultMeta

instance CstrSiteNode Program where
    cstrSiteConstructor = ProgramCstrSite defaultProgramMeta

step :: MonadState DecompositionState m => m ()
step = do
    textOffset <- use #textOffset
    -- c <- guse #cstrSiteOffset
    -- traceM ("step t: " <> show textOffset <> " c: " <> show c)
    when (textOffset /= -1) (#textOffset -= 1)
    when (textOffset > 0) (#cstrSiteOffset += 1)

modifyNodeAt :: forall m.
    MonadError (Doc Annotation) m
    => (Int -> CstrSite -> m CstrSite)
    -> Int
    -> Program
    -> m Program
modifyNodeAt f cursorOffset program
    = runModification >>= \(newProgram, decompositionState) -> if
        | view #textOffset decompositionState > 0 -> throwError
            ("Internal error: Failed to decompose AST for cursor offset "
             <> show cursorOffset)
        | Todo <- view #modificationStatus decompositionState ->
            throwError "Internal error: failed to modify construction site"
        | otherwise -> pure newProgram
  where
    runModification
        = initialDecompositionState cursorOffset
        `usingStateT` runReaderT
            (mapNode program)
            DecompositionEnv { mapChar
                             , mapIdentifier  = mapNode
                             , mapExpr        = mapNode
                             , mapDecl        = mapNode
                             , mapWhereClause = mapNode
                             }
    mapChar c = c <$ step -- trace (show c) step
    -- mapNode :: (Decomposable n, CstrSiteNode n)
    --     => n
    --     -> DecompositionMonad (StateT DecompositionState m) n
    mapNode n = do
        -- t <- guse #textOffset
        -- traceM ("pre " <> take 20 (show n) <> " " <> show t)
        ifM (guses #textOffset (< 0)) (pure n) -- node is located after cursor
            $ do
                #cstrSiteOffset += 1 -- At the moment, the construction site offset passed to `f` is immediately after a node where applying `f` failed. This is not a good default.
                withLocal #cstrSiteOffset 0 $ do
                    newNode <- mapMComponents n
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
                        (tryTransform n)
                        (pure newNode)
    tryTransform :: (Decomposable n, CstrSiteNode n)
        => n
        -> DecompositionMonad (StateT DecompositionState m) n
    tryTransform n
        = catchError
            (lift2 . fmap cstrSiteConstructor
             =<< (f <$> guse #cstrSiteOffset ?? decomposed n)
             <* assign #modificationStatus Success)
        $ const (pure n)

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

instance Decomposable CstrSite where
    decomposed = id
    mapMComponents
        = traverseOf
            (_CstrSite % traversed)
            (join . flap (bitraverse <$> gview #mapChar ?? mapMComponents))

instance Decomposable Node where
    decomposed (IdentifierNode n) = decomposed n
    decomposed (ExprNode n) = decomposed n
    decomposed (DeclNode n) = decomposed n
    decomposed (WhereNode n) = decomposed n
    mapMComponents n = join . asks $ \DecompositionEnv{..} -> case n of
        IdentifierNode
            identifier -> IdentifierNode <$> mapIdentifier identifier
        ExprNode expr -> ExprNode <$> mapExpr expr
        DeclNode decl -> DeclNode <$> mapDecl decl
        WhereNode whereClause -> WhereNode <$> mapWhereClause whereClause

instance Decomposable Identifier where
    decomposed (Identifier name)
        = CstrSite . fromList . map Left $ toString name
    decomposed (IdentifierCstrSite materials) = materials
    mapMComponents identifier@(Identifier _)
        = join
        $ flap
            (traverseOf (_Identifier % unpacked % traversed) <$> gview #mapChar)
            identifier
    mapMComponents (IdentifierCstrSite materials)
        = IdentifierCstrSite <$> mapMComponents materials

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
    mapMComponents = join . flap (asks go)
      where
        go DecompositionEnv{..} e = e & case e of
            _
                | e ^. exprMeta % #parenthesisLevels > 0 -> chain
                    [ (<$ mapChar '(')
                    , whitespaceFragmentTraversal _head %%~ mapChar
                    , refracting
                          (exprMeta % #parenthesisLevels)
                          (subtracting 1)
                      % refracting
                          (exprMeta % #standardMeta % #interstitialWhitespace)
                          (_tail % _init)
                      %%~ mapExpr
                    , whitespaceFragmentTraversal _last %%~ mapChar
                    , (<$ mapChar ')')
                    ]
            -- All these cases could be composed into 1, because the lenses don't overlap, but this is better for totality checking
            Variable _ _ -> _Variable % _2 %%~ mapIdentifier
            Abstraction{} -> chain
                $ intersperseWhitespaceTraversals'
                    [ (<$ mapChar '\\')
                    , _Abstraction % _2 %%~ mapIdentifier
                    , (<$ mapChar '=')
                    , _Abstraction % _3 %%~ mapExpr
                    ]
            Application{} -> chain
                $ intersperseWhitespaceTraversals'
                    [ _Application % _2 %%~ mapExpr
                    , _Application % _3 %%~ mapExpr
                    ]
            Sum{} -> chain
                $ intersperseWhitespaceTraversals'
                    [ _Sum % _2 %%~ mapExpr
                    , (<$ mapChar '+')
                    , _Sum % _3 %%~ mapExpr
                    ]
            ExprCstrSite{} -> _ExprCstrSite % _2 %%~ mapMComponents
          where
            intersperseWhitespaceTraversals'
                = intersperseWhitespaceTraversals mapChar e

instance Decomposable Decl where
    decomposed Decl{..}
        = intersperseWhitespace'
            (interstitialWhitespace meta)
            [ Right $ IdentifierNode name, Left "=", Right $ ExprNode value ]
    decomposed (DeclCstrSite _ materials) = materials
    mapMComponents decl@Decl{} = join . asks $ \DecompositionEnv{..} -> chain
        (intersperseWhitespaceTraversals
             mapChar
             decl
             [ traverseOf #name mapIdentifier
             , (<$ mapChar '=')
             , traverseOf #value mapExpr
             ])
        decl
    mapMComponents (DeclCstrSite meta materials)
        = DeclCstrSite meta <$> mapMComponents materials

instance Decomposable WhereClause where
    decomposed (WhereClause meta decls)
        = intersperseWhitespace'
            (interstitialWhitespace meta)
            (Left "where" : map (Right . DeclNode) (toList decls))
    decomposed (WhereCstrSite _ materials) = materials
    mapMComponents whereClause@(WhereClause _ decls)
        = join . asks $ \DecompositionEnv{..} -> chain
            (intersperseWhitespaceTraversals
                 mapChar
                 whereClause
                 ((<$ traverse_ @[] mapChar "where")
                  : imap
                      (\i _ -> _WhereClause % _2 % ix i %%~ mapDecl)
                      (toList decls)))
            whereClause
    mapMComponents (WhereCstrSite meta materials)
        = WhereCstrSite meta <$> mapMComponents materials

instance Decomposable Program where
    decomposed Program{..}
        = intersperseWhitespace'
            (meta ^. #standardMeta % #interstitialWhitespace)
            (Right (ExprNode expr)
             : (Right . WhereNode <$> maybeToList whereClause)
             ++ [ Left . toString $ view #trailingWhitespace meta ])
    decomposed (ProgramCstrSite _ materials) = materials
    mapMComponents
        program@Program{} = join . asks $ \DecompositionEnv{..} -> chain
        (intersperseWhitespaceTraversals
             mapChar
             program
             [ traverseOf #expr mapExpr
             , #whereClause % _Just %%~ mapWhereClause
             ]
         :> (#meta % #trailingWhitespace % unpacked % traversed %%~ mapChar))
        program
    mapMComponents (ProgramCstrSite meta materials)
        = ProgramCstrSite meta <$> mapMComponents materials
