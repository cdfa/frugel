{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Frugel.Internal.PrettyPrinting where

import           Control.Lens.Plated

import           Data.Data

import           Frugel.PrettyPrinting

import           Optics

import           Prettyprinter.Internal.Debug

deriving instance Eq ann => Eq (Diag ann)

deriving instance Data ann => Data (Diag ann)

deriving instance Data PageWidth

instance Data ann => Plated (Diag ann)

-- equality of docs with constructors with functions such as Nesting, Column and WithPageWidth is undefined
instance (Data ann, Eq ann, Show ann) => Eq (Doc ann) where
    (==) = (==) `on` (checkForIllegalDocs . diag)
      where
        prettyIllegalDocSamples name argName samples
            = "The following '"
            <> name
            <> "'s were found:"
            <> line
            <> angles argName
            <+> "<result>"
            <> line
            <> vsep
                (map
                     (vsep
                      . map (uncurry (surround "\t") . bimap viaShow viaShow))
                     samples)
            <> line
        checkForIllegalDocs doc
            = if illegalDocs == ([], [], [])
                then doc
                else error
                    $ show
                        ("Found illegal Doc while checking equality for"
                         `nestingLine` show doc
                         <> line
                         <> prettyIllegalDocSamples
                             "Column"
                             "start column"
                             columns
                         <> prettyIllegalDocSamples
                             "WithPageWidth"
                             "page width"
                             withPageWidths
                         <> prettyIllegalDocSamples
                             "Nesting"
                             "nesting level"
                             nestings)
          where
            illegalDocs@(columns, withPageWidths, nestings)
                = foldl' (\acc -> \case
                              Column samples -> acc & _1 %~ cons samples
                              WithPageWidth samples -> acc & _2 %~ cons samples
                              Nesting samples -> acc & _3 %~ cons samples
                              _ -> acc) ([], [], []) $ universe doc
