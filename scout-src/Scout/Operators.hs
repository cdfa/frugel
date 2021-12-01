{-# LANGUAGE DeriveDataTypeable #-}

module Scout.Operators ( module Scout.Operators, Associativity(..) ) where

import Control.ValidEnumerable

import Data.Data         hiding ( Fixity, Prefix )
import Data.GenValidity

import GHC.Generics      ( Associativity(..) )

import PrettyPrinting.Expr

import Test.QuickCheck

data UnaryOperator = Negate
    deriving ( Eq, Show, Ord, Data, Generic )

data BinaryOperator
    = Plus
    | Minus
    | Times
    | Division
    | Modulo
    | Equals
    | NotEquals
    | LessThan
    | GreaterThan
    | LessOrEqual
    | GreaterOrEqual
    | And
    | Or
    deriving ( Eq, Show, Ord, Data, Generic )

instance Validity UnaryOperator

instance Validity BinaryOperator

instance GenValid UnaryOperator where
    genValid = sized uniformValid
    shrinkValid = shrinkValid

instance GenValid BinaryOperator where
    genValid = sized uniformValid
    shrinkValid = shrinkValid

instance ValidEnumerable UnaryOperator where
    enumerateValid = datatype [ c0 Negate ]

instance ValidEnumerable BinaryOperator where
    enumerateValid
        = datatype [ c0 Plus
                   , c0 Minus
                   , c0 Times
                   , c0 Division
                   , c0 Modulo
                   , c0 Equals
                   , c0 NotEquals
                   , c0 LessThan
                   , c0 GreaterThan
                   , c0 LessOrEqual
                   , c0 GreaterOrEqual
                   , c0 And
                   , c0 Or
                   ]

binaryOperatorPrecedence :: [[BinaryOperator]]
binaryOperatorPrecedence
        = [ [ Times, Division ]
          , [ Plus, Minus, Modulo ]
          , [ LessThan, GreaterThan, LessOrEqual, GreaterOrEqual ]
          , [ Equals, NotEquals ]
          , [ And ]
          , [ Or ]
          ]

unaryOperatorSymbol :: IsString p => UnaryOperator -> p
unaryOperatorSymbol Negate = "-"

binaryOperatorSymbol :: IsString p => BinaryOperator -> p
binaryOperatorSymbol Plus = "+"
binaryOperatorSymbol Minus = "-"
binaryOperatorSymbol Times = "*"
binaryOperatorSymbol Division = "/"
binaryOperatorSymbol Modulo = "%"
binaryOperatorSymbol Equals = "=="
binaryOperatorSymbol NotEquals = "/="
binaryOperatorSymbol LessThan = "<"
binaryOperatorSymbol GreaterThan = ">"
binaryOperatorSymbol LessOrEqual = "<="
binaryOperatorSymbol GreaterOrEqual = ">="
binaryOperatorSymbol And = "&&"
binaryOperatorSymbol Or = "||"

fixity :: UnaryOperator -> Fixity
fixity Negate = Prefix

associativity :: BinaryOperator -> Associativity
associativity Plus = LeftAssociative
associativity Minus = LeftAssociative
associativity Times = LeftAssociative
associativity Division = LeftAssociative
associativity Modulo = LeftAssociative
associativity Equals = NotAssociative
associativity NotEquals = NotAssociative
associativity LessThan = NotAssociative
associativity GreaterThan = NotAssociative
associativity LessOrEqual = NotAssociative
associativity GreaterOrEqual = NotAssociative
associativity And = RightAssociative
associativity Or = RightAssociative
