{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Zipper.Seq where

import qualified Data.Sequence as Seq

import           Optics

data SeqZipper a = SeqZipper { reversedPrefix :: Seq a, suffix :: Seq a }
    deriving ( Eq )

makeFieldLabelsWith noPrefixFieldLabels ''SeqZipper

unzipTo :: Int -> Seq a -> Maybe (SeqZipper a)
unzipTo i xs
    | 0 <= i && i <= length xs, (prefix, suffix) <- Seq.splitAt i xs
        = Just SeqZipper { reversedPrefix = Seq.reverse prefix, .. }
unzipTo _ _ = Nothing

rezip :: SeqZipper a -> Seq a
rezip SeqZipper{..} = Seq.reverse reversedPrefix <> suffix

insert :: a -> SeqZipper a -> SeqZipper a
insert x = #suffix %~ cons x

delete :: SeqZipper a -> Maybe (SeqZipper a)
delete zipper
    | _ :< newSuffix <- suffix zipper = Just zipper { suffix = newSuffix }
delete _ = Nothing
