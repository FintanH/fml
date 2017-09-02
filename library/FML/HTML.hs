{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FML.HTML
    ( HTML
    , Attribute (..)
    , openTag
    , textTag
    , commentTag
    , emptyTag
    ) where

import           Data.Fix

newtype Attribute str = Attribute (str, str)
  deriving (Show, Eq, Ord)

data HTMLF str a =
    OpenTag str [Attribute str] a
  | TextTag str
  | CommentTag str
  | EmptyTag
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type HTML str = Fix (HTMLF str)

openTag :: str -> [Attribute str] -> HTML str -> HTML str
openTag name attributes = Fix . OpenTag name attributes

textTag :: str -> HTML str
textTag = Fix . TextTag

commentTag :: str -> HTML str
commentTag = Fix . CommentTag

emptyTag :: HTML str
emptyTag = Fix EmptyTag

isOpenTag :: HTMLF str a -> Bool
isOpenTag (OpenTag _ _) = True
isOpenTag _             = False

isTextTag  :: HTMLF str a -> Bool
isTextTag (TextTag _) = True
isTextTag _           = False

isCommentTag :: HTMLF str a -> Bool
isCommentTag (CommentTag _) = True
isCommentTag _              = False

isEmptyTag :: HTMLF str a -> Bool
isEmptyTag EmptyTag = True
isEmptyTag _        = False
