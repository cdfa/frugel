{-# LANGUAGE LambdaCase #-}

module PrettyPrinting.Expr where

import GHC.Generics ( Associativity(..) )

class Expression e where
    precedence :: e -> Int
    fixity :: e -> Maybe Fixity

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
makeExprPrettyPrinter :: Expression e
    => (s -> s) -- parenthesizing function
    -> ((Fixity -> e -> s) -- pretty print unary expression
        -> (Associativity -> e -> e -> (s, s)) -- pretty print binary expression
        -> e
        -> s) -- generic pretty printer
    -> e -- expression
    -> s -- pretty printed expression with minimal parentheses

makeExprPrettyPrinter parenthesized prettyPrint e = prettyPrint' e
  where
    prettyPrint' = prettyPrint prettyPrintUnary prettyPrintBinary
    opPrecedence = precedence e
    prettyPrintUnary opFixity subExp
        = parenthesize (succ opPrecedence)
                       (fixity subExp == Just opFixity)
                       subExp
    prettyPrintBinary associativity left right
        = (\(maxLeftPrecedence, maxRightPrecedence) ->
           ( parenthesize maxLeftPrecedence (fixity left == Just Postfix) left
           , parenthesize maxRightPrecedence
                          (fixity right == Just Prefix)
                          right
           )) $ maxPrecedenceByAssociativity associativity
    maxPrecedenceByAssociativity = \case
        LeftAssociative -> (succ opPrecedence, opPrecedence)
        RightAssociative -> (opPrecedence, succ opPrecedence)
        NotAssociative -> (opPrecedence, opPrecedence)
    parenthesize maxPrecedence fixityObviatesParens subExp
        = if precedence subExp >= maxPrecedence && not fixityObviatesParens
          then parenthesized $ prettyPrint' subExp
          else prettyPrint' subExp
