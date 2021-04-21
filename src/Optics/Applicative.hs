-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Optics.Applicative where

import           Optics.External

infixl 4 <*^>

-- I think it's possible to make this append indices as well, but that would require some magic (probably conjoined) and it's not needed yet
class ApplicativeOptic m where
    (<*^>) :: (m ~ Join k (Join l A_Getter), Is k m, Is l m)
        => Optic' k is s (a -> b)
        -> Optic' l is s a
        -> Optic' m NoIx s b

instance ApplicativeOptic An_AffineFold where
    f <*^> o = afolding (($) <<$>> preview f <<*>> preview o)

instance ApplicativeOptic A_Getter where
    f <*^> o = to (($) <$> view f <*> view o)
