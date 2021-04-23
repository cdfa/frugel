{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE TupleSections #-}

{-# LANGUAGE TypeApplications #-}

module Frugel.Decomposition
    ( module Frugel.Internal.DecompositionState
    , CstrMaterialsZipper
    , Decomposable(..)
    , modifyNodeAt
    ) where

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
import           Frugel.Meta
import           Frugel.Node
import           Frugel.PrettyPrinting
import           Frugel.Program

import           Numeric.Optics

import           Optics

type CstrMaterialsZipper = SeqZipper (Either Char Node)

class Decomposable n where
    decomposed :: n -> CstrMaterials
    -- It would make sense for this function to have a mapKeyword :: Text -> f () and mapWhitespace :: Char -> f Char as well, but it's not yet needed
    -- With writing more boilerplate, it would also be possible to generalize this for applicative functors instead of monads
    -- Except for the Monad constraint, this function is like a monomorphic Bitraversable instance
    mapMComponents :: n -> DecompositionMonad n

class CstrSiteNode n where
    cstrSiteConstructor :: CstrMaterials -> n

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
    when
        (textOffset /= -1)
        (#textOffset -= 1 >> when (textOffset /= 0) (#cstrSiteOffset += 1))

-- TODO: use Except monad
modifyNodeAt :: (Int -> CstrMaterials -> Either [Doc Annotation] CstrMaterials)
    -> Int
    -> Program
    -> Either [Doc Annotation] Program
modifyNodeAt f cursorOffset program
    = if view #textOffset decompositionState > 0
        then Left
            $ one
                ("Failed to decompose AST for cursor offset "
                 <> show cursorOffset)
        else Right newProgram
  where
    (newProgram, decompositionState)
        = initialDecompositionState cursorOffset
        `usingState` runReaderT
            (mapNode program)
            DecompositionEnv { mapChar
                             , mapIdentifier  = mapNode
                             , mapExpr        = mapNode
                             , mapDecl        = mapNode
                             , mapWhereClause = mapNode
                             }
    mapChar c = c <$ step -- trace (show c) step
    -- mapNode :: ( MonadState DecompositionState m
    --            , MonadReader (DecompositionEnv m) m
    --            , Decomposable n
    --            , CstrSiteNode n
    --            )
    --     => n
    --     -> m n
    mapNode n = do
        -- t <- guse #textOffset
        -- traceM ("pre " <> take 20 (show n) <> " " <> show t)
        ifM (guses #textOffset (< 0)) (pure n) -- node is located after cursor
            $ do
                #cstrSiteOffset += 1
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
                        ((&&) <$> guses #textOffset (<= 0)
                         <*> guses #modificationStatus (isn't _Success))
                        (guses #cstrSiteOffset f >>= tryTransform n)
                        (pure newNode)
    tryTransform
        :: (MonadState DecompositionState m, Decomposable n, CstrSiteNode n)
        => n
        -> (CstrMaterials -> Either [Doc Annotation] CstrMaterials)
        -> m n
    tryTransform n f'
        = uncurry (<$)
        . second (assign #modificationStatus)
        . either ((n, ) . Errors) (, Success)
        . second cstrSiteConstructor
        . f'
        $ decomposed n

intersperseWhitespace' :: [Text] -> CstrMaterials' -> CstrMaterials
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

instance Decomposable CstrMaterials where
    decomposed = id
    mapMComponents
        = traverseOf
            (_CstrMaterials % traversed)
            (join . flap (bitraverse <$> gview #mapChar ?? mapMComponents))

instance Decomposable Node where
    decomposed (IdentifierNode n) = decomposed n
    decomposed (ExprNode n) = decomposed n
    decomposed (DeclNode n) = decomposed n
    decomposed (WhereNode n) = decomposed n
    mapMComponents (IdentifierNode n) = IdentifierNode <$> mapMComponents n
    mapMComponents (ExprNode n) = ExprNode <$> mapMComponents n
    mapMComponents (DeclNode n) = DeclNode <$> mapMComponents n
    mapMComponents (WhereNode n) = WhereNode <$> mapMComponents n

instance Decomposable Identifier where
    decomposed (Identifier name)
        = CstrMaterials . fromList . map Left $ toString name
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
        | e ^. exprMeta % #parenthesisLevels > 0
            = toCstrMaterials
                [ Left "("
                , whitespaceFragment leadingFragment
                , e
                  & exprMeta % #parenthesisLevels -~ 1
                  & exprMeta % #standardMeta % #interstitialWhitespace
                  .~ middleWhitespaceFragments
                  & ExprNode
                  & Right
                , whitespaceFragment trailingFragment
                , Left ")"
                ]
      where
        whitespaceFragment = Left . toString
        (leadingFragment, (middleWhitespaceFragments, trailingFragment))
            = fromMaybe
                (error
                     ("Encountered incorrect number of whitespace fragments in "
                      <> show e))
            $ preview
                (exprMeta
                 % #standardMeta
                 % #interstitialWhitespace
                 % _Cons
                 % ((,) <$^> _1 <*^> _2 % _Snoc))
                e
    decomposed e
        = intersperseWhitespace'
            (e ^. exprMeta % #standardMeta % #interstitialWhitespace)
        $ decomposed' e
      where
        decomposed' (IdentifierExpr _ name) = [ Right $ IdentifierNode name ]
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
        decomposed' (ExprCstrSite _ (CstrMaterials materials))
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
            IdentifierExpr _ _ -> _IdentifierExpr % _2 %%~ mapIdentifier
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
