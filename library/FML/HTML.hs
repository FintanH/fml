{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FML.HTML
    ( HTML
    , openTag
    , textTag
    , commentTag
    ) where

import           Data.Fix

newtype Attribute str = Attribute (str, str)
  deriving (Show, Eq, Ord)

data HTMLF str a =
    TagOpen str [Attribute str] a
  | TagText str
  | TagComment str
  deriving (Functor, Show, Eq, Ord, Foldable, Traversable)

type HTML str = Fix (HTMLF str)

openTag :: str -> [Attribute str] -> HTML str -> HTML str
openTag name attributes = Fix . TagOpen name attributes

textTag :: str -> HTML str
textTag = Fix . TagText

commentTag :: str -> HTML str
commentTag = Fix . TagComment
