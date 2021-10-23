{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Optics.Intro where

import Optics

class Intro k is where
    intro :: (ViewableOptic k r) => (a -> ViewResult k r) -> Optic' k is a r

instance Intro A_Getter NoIx where
    intro = to

instance Intro An_AffineFold NoIx where
    intro = afolding

instance Intro A_Fold NoIx where
    intro = castOptic @A_Fold . to
