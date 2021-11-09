{-# LANGUAGE LambdaCase #-}

module PrettyPrinting.Expr where

import GHC.Generics ( Associativity(..) )

class Expression e where
    -- higher precedence binds weaker (because you shouldn't be able to bind stronger than a literal)
    precedence :: e -> Int
    fixity :: e -> Maybe Fixity
    associativity :: e -> Maybe Associativity

    -- fixity Abstraction{} = Just Prefix
    -- fixity _ = Nothing
data Fixity = Prefix | Infix | Postfix
    deriving ( Eq, Show )

-- Finds the precedence of an operator `op` in a nested list of operators
-- The result should be modified according to the precedence of the arity of operators when using with makeExprPrettyPrinter
-- precedence :: Eq o => o -> [[o]] -> Int
-- precedence op = fromJust . findIndex isJust . map (elemIndex op)
{--}
-- pretty prints an expression with minimal parentheses according to
-- precedence function `precedence`,
-- parenthesizing function `parenthesized`
-- and pretty printing function `prettyPrint`
makeExprPrettyPrinter :: (Expression e)
    => (e -> e) -- parenthesizing function
    -> ( e
         -> e
         -> e
       , e
         -> e
         -> e
         -> (e, e) -- pretty print binary expression
       )
makeExprPrettyPrinter parenthesized = (prettyPrintUnary, prettyPrintBinary)
  where
    prettyPrintUnary e subExp
        = parenthesize (succ $ precedence e) (fixity e == fixity subExp) subExp
    prettyPrintBinary e left right
        = (\(maxLeftPrecedence, maxRightPrecedence) ->
           ( parenthesize maxLeftPrecedence False left
           , parenthesize maxRightPrecedence False right
           )) $ maxPrecedenceByAssociativity e
    maxPrecedenceByAssociativity e
        = flip (maybe
                $ error "prettyPrintBinary used with non-binary expression (associativity e === Nothing)")
               (associativity e)
        $ \case
            LeftAssociative -> (succ opPrecedence, opPrecedence)
            RightAssociative -> (opPrecedence, succ opPrecedence)
            NotAssociative -> (opPrecedence, opPrecedence)
      where
        opPrecedence = precedence e
    parenthesize maxPrecedence fixityObviatesParens subExp
        = if precedence subExp >= maxPrecedence && not fixityObviatesParens
          then parenthesized subExp
          else subExp
