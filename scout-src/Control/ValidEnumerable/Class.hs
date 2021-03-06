module Control.ValidEnumerable.Class
    ( module Control.ValidEnumerable.Class
    , datatype
    , share
    , Shared
    , Shareable
    , Typeable
    ) where

import Control.Enumerable  ( Infinite, datatype )
import Control.Sized

import Data.ClassSharing

import Test.QuickCheck.Modifiers

class Typeable a => ValidEnumerable a where
    enumerateValid :: (Typeable f, Sized f) => Shared f a

accessValid :: (ValidEnumerable a, Sized f, Typeable f) => Shareable f a
accessValid = unsafeAccess enumerateValid

c0 :: Sized f => a -> Shareable f a
c0 = pure

c1 :: (ValidEnumerable a, Sized f, Typeable f) => (a -> x) -> Shareable f x
c1 f = fmap f accessValid

c2 :: (ValidEnumerable a, ValidEnumerable b, Sized f, Typeable f)
   => (a -> b -> x)
   -> Shareable f x
c2 f = c1 (uncurry f)

c3 :: ( ValidEnumerable a
      , ValidEnumerable b
      , ValidEnumerable c
      , Sized f
      , Typeable f
      )
   => (a -> b -> c -> x)
   -> Shareable f x
c3 f = c2 (uncurry f)

c4 :: ( ValidEnumerable a
      , ValidEnumerable b
      , ValidEnumerable c
      , ValidEnumerable d
      , Sized f
      , Typeable f
      )
   => (a -> b -> c -> d -> x)
   -> Shareable f x
c4 f = c3 (uncurry f)

c5 :: ( ValidEnumerable a
      , ValidEnumerable b
      , ValidEnumerable c
      , ValidEnumerable d
      , ValidEnumerable e
      , Sized f
      , Typeable f
      )
   => (a -> b -> c -> d -> e -> x)
   -> Shareable f x
c5 f = c4 (uncurry f)

c6 :: ( ValidEnumerable a
      , ValidEnumerable b
      , ValidEnumerable c
      , ValidEnumerable d
      , ValidEnumerable e
      , ValidEnumerable g
      , Sized f
      , Typeable f
      )
   => (a -> b -> c -> d -> e -> g -> x)
   -> Shareable f x
c6 f = c5 (uncurry f)

c7 :: ( ValidEnumerable a
      , ValidEnumerable b
      , ValidEnumerable c
      , ValidEnumerable d
      , ValidEnumerable e
      , ValidEnumerable g
      , ValidEnumerable h
      , Sized f
      , Typeable f
      )
   => (a -> b -> c -> d -> e -> g -> h -> x)
   -> Shareable f x
c7 f = c6 (uncurry f)

instance ValidEnumerable Bool where
    enumerateValid = datatype [ c0 False, c0 True ]

instance Infinite integer => ValidEnumerable (NonNegative integer) where
    enumerateValid = share (NonNegative . fromInteger <$> naturals)

instance (ValidEnumerable a, ValidEnumerable b) => ValidEnumerable (a, b) where
    enumerateValid = share $ pair accessValid accessValid

instance ( ValidEnumerable a
         , ValidEnumerable b
         , ValidEnumerable c
         , ValidEnumerable d
         , ValidEnumerable e
         ) => ValidEnumerable (a, b, c, d, e) where
    enumerateValid = share $ c1 $ \(a, (b, (c, (d, e)))) -> (a, b, c, d, e)

instance (ValidEnumerable a, ValidEnumerable b)
    => ValidEnumerable (Either a b) where
    enumerateValid = datatype [ c1 Left, c1 Right ]

instance ValidEnumerable a => ValidEnumerable [a] where
    enumerateValid = datatype [ pure [], c2 (:) ]

instance ValidEnumerable a => ValidEnumerable (Maybe a) where
    enumerateValid = datatype [ pure Nothing, c1 Just ]

instance ValidEnumerable a => ValidEnumerable (Seq a) where
    enumerateValid = share (fromList <$> accessValid)

instance ValidEnumerable a => ValidEnumerable (NonEmpty a) where
    enumerateValid = datatype [ c2 (:|) ]
