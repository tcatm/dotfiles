{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module TwoPaneFixed (
                      TwoPaneFixed (..)
                    ) where

import XMonad hiding (focus)
import XMonad.StackSet ( focus, up, down)

data TwoPaneFixed a =
    TwoPaneFixed Rational
    deriving ( Show, Read )

instance LayoutClass TwoPaneFixed a where
    doLayout (TwoPaneFixed split) r s = return (arrange r s,Nothing)
        where
          arrange rect st = case reverse (up st) of
                              (master:_) -> [(master,left),(focus st,right)]
                              [] -> case down st of
                                      (next:_) -> [(focus st,left),(next,right)]
                                      [] -> [(focus st, rect)]
              where (left, right) = splitHorizontallyBy split rect

    description _ = "TwoPaneFixed"
