{-# LANGUAGE OverloadedStrings #-}
module Pure.Grid.Theme where
import Pure
import Pure.Data.Txt as Txt
import Data.Monoid ((<>))
import Data.Typeable
import Data.Hashable
data GridT = GridT

gridThemeNamespace = toTxt (tyCon (undefined :: GridT)) <> "_" <> toTxt (abs $ hash (typeOf (undefined :: GridT)))