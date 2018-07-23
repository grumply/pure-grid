{-# LANGUAGE OverloadedStrings, PatternSynonyms, DuplicateRecordFields, RecordWildCards, ViewPatterns, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, TemplateHaskell #-}
module Pure.Grid where

import Pure hiding (color,textAlign,verticalAlign,width,Color_)

import Pure.Data.CSS

import qualified Pure.Data.Styles as Styles
import Pure.Data.Styles hiding (width,textAlign,verticalAlign,color)

import Pure.Data.Cond

import Pure.Data.Prop

import qualified Pure.Data.Txt as Txt

import Pure.Theme

import Control.Arrow ((&&&))
import Control.Monad (void)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Monoid ((<>))
import GHC.Generics as G

import Prelude hiding (or)

import Pure.Grid.Theme

widthProp :: Txt -> Txt -> Bool -> Txt
widthProp val widthClass canEqual
    | Txt.null val = def
    | canEqual && val == "equal" = "equal width"
    | otherwise = toTxt val <>> widthClass

multiProp :: [Txt] -> Txt -> Txt
multiProp val key
    | Prelude.null val = def
    | otherwise =
          Txt.unwords
        . Txt.words
        . Txt.unwords
        $ val

(<>>) x y =
  case (Txt.null x, Txt.null y) of
    (False,False) -> x <<>> y
    (False,_) -> x
    (_,False) -> y
    _ -> ""
(<<>) = (<>>)

data Direction = Neither | Vertically | Horizontally | Both
  deriving (Eq,Ord,Generic,Default)
directionClass :: Direction -> Txt -> Txt
directionClass d suff = 
    case d of
        Vertically -> "vertically-" <> suff <<>> suff
        Horizontally -> "horizontally-" <> suff <<>> suff
        Both -> "padded"
        Neither -> ""

data Relaxed = Unrelaxed | Very | Simply
  deriving (Eq,Ord,Generic,Default)
relaxedClass :: Relaxed -> Txt
relaxedClass Very      = "very-relaxed" <<>> "relaxed"
relaxedClass Simply    = "relaxed"
relaxedClass Unrelaxed = ""

data Celled = NonCelled | Internally | Outlined
  deriving (Eq,Ord,Generic,Default)
celledClass :: Celled -> Txt
celledClass Internally = "interally-celled" <<>> "celled"
celledClass Outlined   = "celled"  
celledClass NonCelled  = ""

data As = As_
pattern As :: HasProp As a => Prop As a -> a -> a
pattern As p a <- (getProp As_ &&& id -> (p,a)) where
    As p a = setProp As_ p a

data Celled = Celled_
pattern Celled :: HasProp Celled a => Prop Celled a -> a -> a
pattern Celled p a <- (getProp Celled_ &&& id -> (p,a)) where
    Celled p a = setProp Celled_ p a

data Centered = Centered_
pattern Centered :: HasProp Centered a => Prop Centered a -> a -> a
pattern Centered p a <- (getProp Centered_ &&& id -> (p,a)) where
    Centered p a = setProp Centered_ p a

data Columns = Columns_
pattern Columns :: HasProp Columns a => Prop Columns a -> a -> a
pattern Columns p a <- (getProp Columns_ &&& id -> (p,a)) where
    Columns p a = setProp Columns_ p a

data IsContainer = IsContainer_
pattern IsContainer :: HasProp IsContainer a => Prop IsContainer a -> a -> a
pattern IsContainer p a <- (getProp IsContainer_ &&& id -> (p,a)) where
    IsContainer p a = setProp IsContainer_ p a

data Divided = Divided_
pattern Divided :: HasProp Divided a => Prop Divided a -> a -> a
pattern Divided p a <- (getProp Divided_ &&& id -> (p,a)) where
    Divided p a = setProp Divided_ p a

data Doubling = Doubling_
pattern Doubling :: HasProp Doubling a => Prop Doubling a -> a -> a
pattern Doubling p a <- (getProp Doubling_ &&& id -> (p,a)) where
    Doubling p a = setProp Doubling_ p a

data Inverted = Inverted_
pattern Inverted :: HasProp Inverted a => Prop Inverted a -> a -> a
pattern Inverted p a <- (getProp Inverted_ &&& id -> (p,a)) where
    Inverted p a = setProp Inverted_ p a

data Padded = Padded_
pattern Padded :: HasProp Padded a => Prop Padded a -> a -> a
pattern Padded p a <- (getProp Padded_ &&& id -> (p,a)) where
    Padded p a = setProp Padded_ p a

data Relaxed = Relaxed_
pattern Relaxed :: HasProp Relaxed a => Prop Relaxed a -> a -> a
pattern Relaxed p a <- (getProp Relaxed_ &&& id -> (p,a)) where
    Relaxed p a = setProp Relaxed_ p a

data Reversed = Reversed_
pattern Reversed :: HasProp Reversed a => Prop Reversed a -> a -> a
pattern Reversed p a <- (getProp Reversed_ &&& id -> (p,a)) where
    Reversed p a = setProp Reversed_ p a

data Stackable = Stackable_
pattern Stackable :: HasProp Stackable a => Prop Stackable a -> a -> a
pattern Stackable p a <- (getProp Stackable_ &&& id -> (p,a)) where
    Stackable p a = setProp Stackable_ p a

data Stretched = Stretched_
pattern Stretched :: HasProp Stretched a => Prop Stretched a -> a -> a
pattern Stretched p a <- (getProp Stretched_ &&& id -> (p,a)) where
    Stretched p a = setProp Stretched_ p a

data TextAlign = TextAlign_
pattern TextAlign :: HasProp TextAlign a => Prop TextAlign a -> a -> a
pattern TextAlign p a <- (getProp TextAlign_ &&& id -> (p,a)) where
    TextAlign p a = setProp TextAlign_ p a

data VerticalAlign = VerticalAlign_
pattern VerticalAlign :: HasProp VerticalAlign a => Prop VerticalAlign a -> a -> a
pattern VerticalAlign p a <- (getProp VerticalAlign_ &&& id -> (p,a)) where
    VerticalAlign p a = setProp VerticalAlign_ p a

data Color = Color_
pattern Color :: HasProp Color a => Prop Color a -> a -> a
pattern Color p a <- (getProp Color_ &&& id -> (p,a)) where
    Color p a = setProp Color_ p a

data Floated = Floated_
pattern Floated :: HasProp Floated a => Prop Floated a -> a -> a
pattern Floated p a <- (getProp Floated_ &&& id -> (p,a)) where
    Floated p a = setProp Floated_ p a

data OnComputer = OnComputer_
pattern OnComputer :: HasProp OnComputer a => Prop OnComputer a -> a -> a
pattern OnComputer p a <- (getProp OnComputer_ &&& id -> (p,a)) where
    OnComputer p a = setProp OnComputer_ p a

data OnLargeScreen = OnLargeScreen_
pattern OnLargeScreen :: HasProp OnLargeScreen a => Prop OnLargeScreen a -> a -> a
pattern OnLargeScreen p a <- (getProp OnLargeScreen_ &&& id -> (p,a)) where
    OnLargeScreen p a = setProp OnLargeScreen_ p a

data Only = Only_
pattern Only :: HasProp Only a => Prop Only a -> a -> a
pattern Only p a <- (getProp Only_ &&& id -> (p,a)) where
    Only p a = setProp Only_ p a

data OnMobile = OnMobile_
pattern OnMobile :: HasProp OnMobile a => Prop OnMobile a -> a -> a
pattern OnMobile p a <- (getProp OnMobile_ &&& id -> (p,a)) where
    OnMobile p a = setProp OnMobile_ p a

data OnTablet = OnTablet_
pattern OnTablet :: HasProp OnTablet a => Prop OnTablet a -> a -> a
pattern OnTablet p a <- (getProp OnTablet_ &&& id -> (p,a)) where
    OnTablet p a = setProp OnTablet_ p a

data OnWidescreen = OnWidescreen_
pattern OnWidescreen :: HasProp OnWidescreen a => Prop OnWidescreen a -> a -> a
pattern OnWidescreen p a <- (getProp OnWidescreen_ &&& id -> (p,a)) where
    OnWidescreen p a = setProp OnWidescreen_ p a

data Width = Width_
pattern Width :: HasProp Width a => Prop Width a -> a -> a
pattern Width p a <- (getProp Width_ &&& id -> (p,a)) where
    Width p a = setProp Width_ p a

pattern Equal = "equal"
pattern One = "one"
pattern Two = "two"
pattern Three = "three"
pattern Four = "four"
pattern Five = "five"
pattern Six = "six"
pattern Seven = "seven"
pattern Eight = "eight"
pattern Nine = "nine"
pattern Ten = "ten"
pattern Eleven = "eleven"
pattern Twelve = "twelve"
pattern Thirteen = "thirteen"
pattern Fourteen = "fourteen"
pattern Fifteen = "fifteen"
pattern Sixteen = "sixteen"

instance Themeable GridT where
    theme _ _ = $(mkRawCSS $ do
        let c = "." <> gridThemeNamespace
            cs = ["one","two","three","four","five","six","seven","eight"
                 ,"nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen"
                 ]
        is c .> do
            display =: "-webkit-box"
            display =: "-ms-flexbox"
            display =: "flex"
            "-webkit-box-orient" =: horizontal
            "-webkit-box-direction" =: normal
            "-ms-flex-direction" =: row
            "flex-direction" =: row
            "-ms-flex-wrap" =: wrap
            "flex-wrap" =: wrap
            "-webkit-box-align" =: stretch
            "-ms-flex-align" =: stretch
            "align-items" =: stretch
            padding =: ems 0
            margin =: neg (rems 1)
        is c . is ".relaxed" .> do
            marginLeft =: neg (rems 1.5)
            marginRight =: neg (rems 1.5)
        is c . is ".relaxed" . is ".very-relaxed" .> do
            marginLeft =: neg (rems 2.5)
            marginRight =: neg (rems 2.5)
        is c . next c .>
            marginTop =: rems 1
        is c . child ".column" . isn't ".row" 
          . or is c . child ".row" . child ".column" .> do
            position =: relative
            display =: inlineBlock
            Styles.width =: per 6.25
            paddingLeft =: rems 1
            paddingRight =: rems 1
            Styles.verticalAlign =: top
        is c . child "*" .> do
            paddingLeft =: rems 1
            paddingRight =: rems 1
        is c . child ".row" .> do
            display =: "-webkit-box"
            display =: "-ms-flexbox"
            display =: "flex"
            "-webkit-box-orient" =: horizontal
            "-webkit-box-direction" =: normal
            "-ms-flex-direction" =: row
            "flex-direction" =: row
            "-ms-flex-wrap" =: wrap
            "flex-wrap" =: wrap
            "-webkit-box-pack" =: inherit
            "-ms-flex-pack" =: inherit
            "justify-content" =: inherit
            "-webkit-box-align" =: stretch
            "-ms-flex-align" =: stretch
            "align-items" =: stretch
            important $ Styles.width =: per 100
            padding =: rems 0
            paddingTop =: rems 1
            paddingBottom =: rems 1
        is c . child ".column" . isn't ".row" .> do
            paddingTop =: rems 1
            paddingBottom =: rems 1
        is c . child ".row" . child ".column" .> do
            marginTop =: ems 0
            marginBottom =: ems 0
        is c . child ".row" . child "img" 
          . or is c . child ".row" . child ".column" . child "img" .>
            maxWidth =: per 100
        is c . child c . is ":first-child" .> do
            marginTop =: ems 0
        is c . child c . is ":last-child" .> do
            marginBottom =: ems 0
        for_ [("(max-width: 767px)",ems 0)
                ,("(min-width 768px) and (max-width: 991px)",ems 2)
                ,("(min-width 992px) and (max-width: 1199px)",per 3)
                ,("(min-width 1200px) and (max-width: 1919px)",per 15)
                ,("(min-width 1920px)",per 23)
                ] $ \(med,pad) -> atMedia ("only screen and " <> med) $
                  is c . is ".page" .> do
                    Styles.width =: auto
                    marginLeft =: ems 0
                    marginRight =: ems 0
                    paddingLeft =: pad
                    paddingRight =: pad
        is c . child ".column" . is ":only-child" . or is c . child ".row" . child ".column" . is ":only-child" .>
            Styles.width =: per 100
        for_ cs $ \i -> do
          is c . is i . child ".row" . child ".column" 
            . or is c . is i . child ".column" . isn't ".row" .>
              Styles.width =: per (100 / i)
          is c . child i . is ".row" . child ".column" .> do
              important $ Styles.width =: per (100 / i)
          is c . child ".row" . child i . is ".wide" . is ".column"
            . or is c . child ".column" . is ".row" . child i . is ".wide"  . is ".column"
            . or is c . child i . is ".wide" . is ".column"
            . or is c . is ".column" . child i . is ".wide" . is ".column" .> do
              important $ Styles.width =: per (100 / (fromIntegral 16 - (i - 1)))
        is c . is ".celled" . is ".page" .> do
            "-webkit-box-shadow" =: none
            "box-shadow" =: none
        is c . is ".centered"
          . or is c . is ".centered" . child ".row" 
          . or is c . is ".centered" . is ".row" .> do
            Styles.textAlign =: center
            "-webkit-box-pack" =: center
            "-ms-flex-pack" =: center
            justifyContent =: center
        is c . child ".centered" . is ".column" 
          . or is c . child ".row" . child ".centered" . is ".column" .> do
            display =: block
            marginLeft =: auto
            marginRight =: auto
        is c . is ".relaxed" . child ".column" . isn't ".row" 
          . or is c . is ".relaxed" . child ".row" . child ".column" 
          . or is c . child ".relaxed" . is ".row" . child ".column" .> do
            paddingLeft =: rems 1.5
            paddingRight =: rems 1.5
        is c . is ".very-relaxed" . child ".column" .isn't ".row"
          . or is c . is ".very-relaxed" . child ".row" . child ".column" 
          . or is c . child ".very-relaxed" . is ".row" . child ".column" .> do
            paddingLeft =: rems 2.5
            paddingRight =: rems 2.5
        is c . is ".padded" .> do
            important $ margin =: ems 0
        is c . is ".horizontally-padded" .> do
            important $ marginLeft =: ems 0
            important $ marginRight =: ems 0
        is c . is ".vertically-padded" .> do
            important $ marginTop =: ems 0
            important $ marginBottom =: ems 0
        is c . has ".left" . is ".foated" . is ".column" .> 
            marginRight =: auto
        is c . has ".right" . is ".foated" . is ".column" .> 
            marginLeft =: auto
        is c . is ".divided" . child ".column" . isn't ".row"
          . or is c . is ".horizontally-divided" . child ".column" . isn't ".row"
          . or is c . is ".divided" . child ".row" . child ".column"
          . or is c . is ".horizontally-divided" . child ".row" . child ".column" .> do
            "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
            "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
        is c . is ".vertically-divided" . child ".column" . isn't ".row"
          . or is c . is ".vertically-divided" . child ".row" . child ".column" .> do
            marginTop =: rems 1
            marginBottom =: rems 1
            paddingTop =: rems 0
            paddingBottom =: rems 0
        is c . is ".vertically-divided" . child ".row" .> do
            marginTop =: ems 0
            marginBottom =: ems 0
        is c . is ".divided" . child ".column" . isn't ":first-child" 
          . or is c . is ".horizontally-divided" . child ".column" . isn't ":first-child" 
          . or is c . is ".divided" . child ".row" . child ".column" . isn't ":first-child"
          . or is c . is ".horizontally-divided" . child ".row" . child ".column" . isn't ":first-child" .> do
            "-webkit-box-shadow" =: none
            "box-shadow" =: none
        is c . is ".vertically-divided" . child ".row" . is ":first-child" . child ".column" .>
            marginTop =: ems 0
        is c . child ".divided" . is ".row" . child ".column"
            . or is c . child ".horizontally-divided" . is ".row" . child ".column"
            . or is c . child ".vertically-divided" . is ".row" . child ".column" .> do
            "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
            "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
        child ".divided" . is ".row" . child ".column" . is ":first-child" .> do
            . or is c . child ".horizontally-divided" . is ".row" . child ".column" . is ":first-child" .> do
            . or is c . child ".vertically-divided" . is ".row" . child ".column" . is ":first-child" .> do
            "-webkit-box-shadow" =: none
            "box-shadow" =: none
        is ".vertically-divided" . child ".row" .> 
            position =: relative
        is ".vertically-divided" . child ".row" . is ":before" .> do
            position =: absolute
            content =: emptyQuotes
            top =: ems 0
            left =: pxs 0
            Styles.width =: "calc(" <> per 100 <> " - " <> rems 2 <> ")"
            height =: pxs 1
            margin =: per 0 <<>> rems 1
            "-webkit-box-shadow" =: pxs 0 <<>> neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
            "box-shadow" =: pxs 0 <<>> neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
        is ".divided" . is ".padded"
            . or is c . is ".divided" . is ".horizontally-padded"
            . or is c . is ".divided" . is ".vertically-padded"
            . or is c . is ".vertically-divided" . is ".padded"
            . or is c . is ".vertically-divided" . is ".horizontally-padded"
            . or is c . is ".vertically-divided" . is ".vertically-padded"
            . or is c . is ".horizontally-divided" . is ".padded"
            . or is c . is ".horizontally-divided" . is ".horizontally-padded"
            . or is c . is ".horizontally-divided" . is ".vertically-padded" .>
            Styles.width =: per 100
        is ".vertically-divided" . child ".row" . is ":first-child" . is ":before" .> do
            "-webkit-box-shadow" =: none
            "box-shadow" =: none
        is ".inverted" . is ".divided" . child ".column" . isn't ".row"
            . or is c . is ".inverted" . is ".horizontally-divided" . child ".column" . isn't ".row"
            . or is c . is ".inverted" . is ".divided" . child ".row" . child ".column"
            . or is c . is ".inverted" . is ".horizontally-divided" . child ".row" . child ".column" .> do
            "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(255,255,255,0.1)
            "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(255,255,255,0.1)
        is ".inverted" . is ".divided" . child ".column" . isn't ".row" . is ":first-child"
            . or is c . is ".inverted" . is ".horizontally-divided" . child ".column" . isn't ".row" . is ":first-child"
            . or is c . is ".inverted" . is ".divided" . child ".row" . child ".column" . is ":first-child" 
            . or is c . is ".inverted" . is ".horizontally-divided" . child ".row" . child ".column" . is ":first-child" .> do
            "-webkit-box-shadow" =: none
            "box-shadow" =: none
        is ".inverted" . is ".divided" . is ".vertically" . child ".row" . is ":before" .> do
            "-webkit-box-shadow" =: pxs 0 <<>> neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> rgba(255,255,255,0.1)
            "box-shadow" =: pxs 0 <<>> neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> rgba(255,255,255,0.1)
        is ".relaxed" . is ".divided" . is ".vertically" . child ".row" . is ":before" .> do
            marginLeft =: rems 1.5
            marginRight =: rems 1.5
            Styles.width =: "calc(" <> per 100 <> " - " <> rems 3 <> ")"
        is ".relaxed" . is ".very" . is ".divided" . is ".vertically" . child ".row" . is ":before" .> do
            marginLeft =: rems 5
            marginRight =: rems 5
            Styles.width =: "calc(" <> per 100 <> " - " <> rems 5 <> ")" -- is this right, shoud it not be rems 10?
        is ".celled" .> do
            Styles.width =: per 100
            margin =: ems 1 <<>> ems 0
            "-webkit-box-shadow" =: pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> pxs 1 <<>> "#D4D4D5"
            "box-shadow" =: pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> pxs 1 <<>> "#D4D4D5"
        is ".celled" . child ".row" .> do
            important $ Styles.width =: per 100
            margin =: ems 0
            padding =: ems 0
            "-webkit-box-shadow" =: pxs 0 <<>> neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> "#D4D4D5"
            "box-shadow" =: pxs 0 <<>> neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> "#D4D4D5"
        is ".celled" . child ".column" . isn't ".row" 
            . or is c . is ".celled" . child ".row" . child ".column" .> do
            "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> "#D4D4D5"
            "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> "#D4D4D5"
        is ".celled" . child ".column" . is ":first-child"
            . or is c . is ".celled" . child ".row" . child ".column" . is ":first-child" .> do
            "-webkit-box-shadow" =: none
            "box-shadow" =: none
        is ".celled" . child ".column" . isn't ".row"
            . or is c . is ".celled" . child ".row" . child ".column" .> 
            padding =: ems 1
        is ".relaxed" . is ".celled" . child ".column" . isn't ".row"
            . or is c . is ".relaxed" . is ".celled" . child ".row" . child ".column" .>
            padding =: ems 1.5
        is ".relaxed" . is ".very" . is ".celled" . child ".column" . isn't ".row"
            . or is c . is ".relaxed" . is ".very" . is ".celled" . child ".row" . child ".column" .>
            padding =: ems 2
        is ".celled" . is ".internally" .> do
            "-webkit-box-shadow" =: none
            "box-shadow" =: none
            margin =: ems 0
        is ".celled" . is ".internally" . child ".row" . is ":first-child" 
            . or is c . is ".celled" . is ".internally" . child ".row" . child ".column" . is ":first-child" .> do
            "-webkit-box-shadow" =: none
            "box-shadow" =: none
        is ".aligned" . is ".top" . child ".column" . isn't ".row"
            . or is c . is ".aligned" . is ".top" . child ".row" . child ".column"
            . or is c . child ".aligned" . is ".top" . is ".row" . child ".column"
            . or is c . child ".aligned" . is ".top" . is ".column" . isn't ".row"
            . or is c . child ".row" . child ".aligned"  . is ".top" . is ".column" .> do
            "-webkit-box-orient" =: vertical
            "-webkit-box-direction" =: normal
            "-ms-flex-direction" =: column
            "flex-direction" =: column
            "vertical-align" =: top
            important $ "-ms-flex-item-align" =: start
            important $ "align-self" =: "flex-start"
        is ".aligned" . is ".middle" . child ".column" . isn't ".row"
            . or is c . is ".aligned" . is ".middle" . child ".row" . child ".column"
            . or is c . child ".aligned" . is ".middle" . is ".row" . child ".column"
            . or is c . child ".aligned" . is ".middle" . is ".column" . isn't ".row"
            . or is c . child ".row" . child ".aligned"  . is ".middle" . is ".column" .> do
            "-webkit-box-orient" =: vertical
            "-webkit-box-direction" =: normal
            "-ms-flex-direction" =: column
            "flex-direction" =: column
            "vertical-align" =: middle
            important $ "-ms-flex-item-align" =: center
            important $ "align-self" =: center
        is ".aligned" . is ".bottom" . child ".column" . isn't ".row"
            . or is c . is ".aligned" . is ".bottom" . child ".row" . child ".column"
            . or is c . child ".aligned" . is ".bottom" . is ".row" . child ".column"
            . or is c . child ".aligned" . is ".bottom" . is ".column" . isn't ".row"
            . or is c . child ".row" . child ".aligned"  . is ".bottom" . is ".column" .> do
            "-webkit-box-orient" =: vertical
            "-webkit-box-direction" =: normal
            "-ms-flex-direction" =: column
            "flex-direction" =: column
            "vertical-align" =: bottom
            important $ "-ms-flex-item-align" =: end
            important $ "align-self" =: "flex-end"           
        is ".stretched" . child ".row" . child ".column"
            . or is c . is ".stretched" . child ".column"
            . or is c . child ".row" . is ".stretched" . child ".column"
            . or is c . child ".column" . is ".stretched" . isn't ".row"
            . or is c . child ".row" . child ".column" . is ".stretched" .> do
            important $ display =: "-webkit-inline-box"
            important $ display =: "-ms-inline-flexbox"
            important $ display =: "inline-flex"
            "-ms-flex-item-align" =: "stretch"
            "align-self" =: "stretch"
            "-webkit-box-orient" =: vertical
            "-webkit-box-direction" =: normal
            "-ms-flex-direction" =: column
            "flex-direction" =: column
        is ".stretched" . child ".row" . child ".column" . child "*"
            . or is c . is ".stretched" . child ".column" . child "*"
            . or is c . child ".row" . is ".stretched" . child ".column" . child "*"
            . or is c . child ".column" . is ".stretched" . isn't ".row"
            . or is c . child ".row" . child ".column" . is ".stretched" . child "*" .> do
            "-webkit-box-flex" =: one
            "-ms-flex-positive" =: one
            "flex-grow" =: one
        is ".aligned" . is ".left" . child ".column"
            . or is c . is ".aligned" . is ".left" . child ".row" . child ".column"
            . or is c . child ".aligned" . is ".left" . is ".row" . child ".column"
            . or is c . child ".aligned" . is ".left" . is ".column" . is ".column"
            . or is c . child ".row" . child ".column"  . is ".column" . is ".aligned" . is ".left" .> do
            "text-align" =: left
            "-ms-flex-item-align" =: inherit
            "align-self" =: inherit
        is ".aligned" . is ".center" . child ".column"
            . or is c . is ".aligned" . is ".center" . child ".row" . child ".column"
            . or is c . child ".aligned" . is ".center" . is ".row" . child ".column"
            . or is c . child ".aligned" . is ".center" . is ".column" . is ".column"
            . or is c . child ".row" . child ".column"  . is ".column" . is ".aligned" . is ".center" .> do
            "text-align" =: center
            "-ms-flex-item-align" =: inherit
            "align-self" =: inherit
        is ".aligned" . is ".center" .> do
            "-webkit-box-pack" =: center
            "-ms-flex-pack" =: center
            "justify-content" =: center
        is ".aligned" . is ".right" . child ".column"
            . or is c . is ".aligned" . is ".right" . child ".row" . child ".column"
            . or is c . child ".aligned" . is ".right" . is ".row" . child ".column"
            . or is c . child ".aligned" . is ".right" . is ".column" . is ".column"
            . or is c . child ".row" . child ".column"  . is ".column" . is ".aligned" . is ".right" .> do
            "text-align" =: right
            "-ms-flex-item-align" =: inherit
            "align-self" =: inherit
        is ".justified" . child ".column"
            . or is c . is ".justified" . child ".row" . child ".column"
            . or is c . child ".row" . is ".justified" . child ".column"
            . or is c . child ".column" . is ".column" . is ".justified"
            . or is c . child ".row" . child ".column" . is ".column" . is ".justified" .> do
            "text-align" =: "justify"
            "-webkit-hyphens" =: auto
            "-ms-hyphens" =: auto
            "hyphens" =: auto
        is ".width" . is ".equal" . child ".column" . isn't ".row"
            . or is c . is ".width" . is ".equal" . child ".row" . child ".column"
            . or is c . child ".row" . is ".width" . is ".equal" . child ".column" .> do
            display =: inlineBlock
            "-webkit-box-flex" =: one
            "-ms-flex-positive" =: one
            "flex-grow" =: one
        is ".width" . is ".equal" . child ".column" . is ".wide"
            . or is c . is ".width" . is ".equal" . child ".row" . child ".column" . is ".wide"
            . or is c . child ".row" . is ".width" . is ".equal" . child ".column" . is ".wide" .> do
            "-webkit-box-flex" =: zero
            "-ms-flex-positive" =: zero
            "flex-grow" =: zero
        atMedia "only screen and (max-width: 767px)" $ do
            is ".reversed" . is ".mobile"
                . or is c . is ".reversed" . is ".mobile" . child ".row"
                . or is c . child ".row" . is ".reversed" . is ".mobile" .> do
                "-webkit-box-orient" =: "horizontal"
                "-webkit-box-direction" =: "reverse"
                "-ms-flex-direction" =: "row-reverse"
                "flex-direction" =: "row-reverse"
            is ".reversed" . is ".vertically" . is ".mobile"
                . or is c . is ".stackable" .  is ".reversed" . is ".mobile" .> do
                "-webkit-box-orient" =: "vertical"
                "-webkit-box-direction" =: "reverse"
                "-ms-flex-direction" =: "column-reverse"
                "flex-direction" =: "column-reverse"
            is ".reversed" . is ".mobile" . is ".divided" . isn't ".vertically-divided" . child ".column" . is ":first-child"
                . or is c . is ".reversed" . is ".mobile" . isn't ".vertically-divided" . child ".row" . child ".column" . is ":first-child" .> do
                "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
                "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,35,38,0.15)
            is ".reversed" . is ".mobile" . is ".divided" . isn't ".vertically-divided" . child ".column" . is ":last-child"
                . or is c . is ".reversed" . is ".mobile" . isn't ".vertically-divided" . child ".row" . child ".column" . is ":last-child" .> do
                "-webkit-box-shadow" =: none
                "box-shadow" =: none
            is ".reversed" . is ".vertically" . is ".mobile" . is ".divided" . is ".vertically-divided" . child ".column" . is ":first-child"
                . or is c . is ".reversed" . is ".vertically" . is ".mobile" . is ".vertically-divided" . child ".row" . child ".column" . is ":first-child" .> do
                "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
                "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,35,38,0.15)
            is ".reversed" . is ".vertically" . is ".mobile" . is ".divided" . is ".vertically-divided" . child ".column" . is ":last-child"
                . or is c . is ".reversed" . is ".vertically" . is ".mobile" . is ".vertically-divided" . child ".row" . child ".column" . is ":last-child" .> do
                "-webkit-box-shadow" =: none
                "box-shadow" =: none
            is ".celled" . is ".reversed" . is ".mobile" . child ".row" . child ".column" . is ":first-child" .> do
                "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> "#D4D4D5"
                "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> "#D4D4D5"
            is ".celled" . is ".reversed" . is ".mobile" . child ".row" . child ".column" . is ":last-child" .> do
                "-webkit-box-shadow" =: none
                "box-shadow" =: none
            child ".row" . is ".doubling" . or is c . is ".doubling" . child ".row" .> do
                important $ margin =: ems 0
                important $ padding =: ems 0
            child ".row"  . is ".doubling" .child ".column" 
                . or is c . is ".doubling" . child ".row" . child ".column" .> do
                important $ paddingTop =: rems 1
                important $ paddingBottom =: rems 1
                important $ "-webkit-box-shadow" =: none
                important $ "box-shadow" =: none
                important $ margin =: ems 0
            for_ (zip [1,2,2,2,2,2,2,3,3,3,3,3,4,4,4] cs) $ \(n,sz) -> do
                is ".column" . is ".doubling" . is sz . child ".row" . child ".column" 
                    . or is c . is ".column" . is ".doubling" . is sz . child ".column" . isn't ".row"
                    . or is c . is ".row" . is ".row" . is ".column" . is ".doubling" . is sz . child ".column" .> do
                    important $ width =: (1 / n)
            is ".stackable" .> do
                width =: auto
                important $ marginLeft =: ems 0
                important $ marginRight =: ems 0
            is ".stackable" . child ".row" . child ".column" . is ".wide"
                . or is c . is ".stackable" . is ".column" . is ".wide"
                . or is c . is ".stackable" . is c . is ".column" . child ".column"
                . or is c . is ".stackable" . child ".column" . is ".row" . child ".column"
                . or is c . is ".stackable" . child ".row" . child ".column"
                . or is c . is ".stackable" . child ".column" . isn't ".row"
                . or is c . child ".row" . is ".stackable" . is ".stackable" . child ".column" .> do
                important $ width =: per 100
                important $ margin =: ems 0 <<>> ems 0
                important $ "-webkit-box-shadow" =: none
                important $ "box-shadow" =: none
                important $ padding =: rems 1 <<>> rems 1
            is ".stackable" . isn't ".vertically" . child ".row" .> do
                margin =: ems 0
                padding =: ems 0
            has c . is ".stackable" .> do
                important $ marginLeft =: neg (rems 1)
                important $ marginRight =: neg (rems 1)
            is ".stackable" . is ".divided" . child ".row" . is ":first-child" . child ".column" . is ":first-child"
                . or is c . is ".stackable" . is ".celled" . child ".row" . is ":first-child" . child ".column" . is ":first-child"
                . or is c . is ".stackable" . is ".divided" . child ".column" . isn't ".row" . is ":first-child"
                . or is c . is ".stackable" . is ".celled" . child ".column" . isn't ".row" .is ":first-child" .>
                important $ borderTop =: none
            is ".stackable" . is ".celled" . is ".inverted" . child ".column" . isn't ".row"
                . or is c . is ".stackable" . is ".divided" . is ".inverted" . child ".column" . isn't ".row"
                . or is c . is ".stackable" . is ".celled" . is ".inverted" . child ".row" . child ".column"
                . or is c . is ".stackable" . is ".divided" . is ".inverted" . child ".row" . child ".column" .>
                important $ borderTop =: pxs 1 <<>> solid <<>> rgba(255,255,255,0.1)
            is ".stackable" . is ".celled" . child ".column" . isn't ".row"
                . or is c . is ".stackable" . is ".divided" . isn't ".vertically" . isn't ".vertically-divided". child ".column" . isn't ".row"
                . or is c . is ".stackable" . is ".celled" . child ".row" . child ".column"
                . or is c . is ".stackable" . is ".divided" . isn't ".vertically"  isn't ".vertically-divided" . child ".row" . child ".column" .> do
                borderTop =: pxs 1 <<>> solid <<>> rgba(34,36,38,0.15)
                important $ "-webkit-box-shadow" =: none
                important $ "box-shadow" =: none
                important $ paddingTop =: rems 2
                important $ paddingBottom =: rems 2
            is ".stackable" . is ".celled" . child ".row" .> do
                important $ "-webkit-box-shadow" =: none
                important $ "-box-shadow" =: none
            is ".stackable" . is ".divided" . 


        atMedia "only screen and (min-width: 768px) and (max-width: 991px)" $ do
            is ".reversed" . is ".tablet"
                . or is c . is ".reversed" . is ".tablet" . child ".row"
                . or is c . child ".row" . is ".reversed" . is ".tablet" .> do
                "-webkit-box-orient" =: "horizontal"
                "-webkit-box-direction" =: "reverse"
                "-ms-flex-direction" =: "row-reverse"
                "flex-direction" =: "row-reverse"
            is ".reversed" . is ".vertically" . is ".tablet"
                . or is c . is ".stackable" .  is ".reversed" . is ".tablet" .> do
                "-webkit-box-orient" =: "vertical"
                "-webkit-box-direction" =: "reverse"
                "-ms-flex-direction" =: "column-reverse"
                "flex-direction" =: "column-reverse"
            is ".reversed" . is ".tablet" . is ".divided" . isn't ".vertically-divided" . child ".column" . is ":first-child"
                . or is c . is ".reversed" . is ".tablet" . isn't ".vertically-divided" . child ".row" . child ".column" . is ":first-child" .> do
                "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
                "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,35,38,0.15)
            is ".reversed" . is ".tablet" . is ".divided" . isn't ".vertically-divided" . child ".column" . is ":last-child"
                . or is c . is ".reversed" . is ".tablet" . isn't ".vertically-divided" . child ".row" . child ".column" . is ":last-child" .> do
                "-webkit-box-shadow" =: none
                "box-shadow" =: none
            is ".celled" . is ".reversed" . is ".tablet" . child ".row" . child ".column" . is ":first-child" .> do
                "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> "#D4D4D5"
                "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> "#D4D4D5"
            is ".celled" . is ".reversed" . is ".tablet" . child ".row" . child ".column" . is ":last-child" .> do
                "-webkit-box-shadow" =: none
                "box-shadow" =: none
            is ".doubling" .> 
                width =: auto
            child ".row" . is ".doubling" . or is c . is ".doubling" . child ".row" .> do
                important $ margin =: ems 0
                important $ padding =: ems 0
            child ".row"  . is ".doubling" .child ".column" 
                . or is c . is ".doubling" . child ".row" . child ".column" .>
                important $ display =: inlineBlock
                important $ paddingTop =: rems 1
                important $ paddingBottom =: rems 1
                important $ "-webkit-box-shadow" =: none
                important $ "box-shadow" =: none
                margin =: ems 0
            for_ (zip [1,2,2,3,3,3,4,4,5,5,6,6,7,7,8] cs) $ \(n,sz) -> do
                is ".column" . is ".doubling" . is sz . child ".row" . child ".column" 
                    . or is c . is ".column" . is ".doubling" . is sz . child ".column" . isn't ".row"
                    . or is c . is ".row" . is ".row" . is ".column" . is ".doubling" . is sz . child ".column" .> do
                    important $ width =: (1 / n)
        atMedia "only screen and (min-width: 992px)" $ do
            is ".reversed" . is ".computer"
                . or is c . is ".reversed" . is ".computer" . child ".row"
                . or is c . child ".row" . is ".reversed" . is ".computer" .> do
                "-webkit-box-orient" =: "horizontal"
                "-webkit-box-direction" =: "reverse"
                "-ms-flex-direction" =: "row-reverse"
                "flex-direction" =: "row-reverse"
            is ".reversed" . is ".vertically" . is ".computer"
                . or is c . is ".stackable" .  is ".reversed" . is ".computer" .> do
                "-webkit-box-orient" =: "vertical"
                "-webkit-box-direction" =: "reverse"
                "-ms-flex-direction" =: "column-reverse"
                "flex-direction" =: "column-reverse"
            is ".reversed" . is ".computer" . is ".divided" . isn't ".vertically-divided" . child ".column" . is ":first-child"
                . or is c . is ".reversed" . is ".computer" . isn't ".vertically-divided" . child ".row" . child ".column" . is ":first-child" .> do
                "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
                "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> rgba(34,35,38,0.15)
            is ".reversed" . is ".computer" . is ".divided" . isn't ".vertically-divided" . child ".column" . is ":last-child"
                . or is c . is ".reversed" . is ".computer" . isn't ".vertically-divided" . child ".row" . child ".column" . is ":last-child" .> do
                "-webkit-box-shadow" =: none
                "box-shadow" =: none
            is ".celled" . is ".reversed" . is ".computer" . child ".row" . child ".column" . is ":first-child" .> do
                "-webkit-box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> "#D4D4D5"
                "box-shadow" =: neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> pxs 0 <<>> "#D4D4D5"
            is ".celled" . is ".reversed" . is ".computer" . child ".row" . child ".column" . is ":last-child" .> do
                "-webkit-box-shadow" =: none
                "box-shadow" =: none
        )

data Grid = Grid_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , celled :: Celled
    , centered :: Bool
    , columns :: Txt
    , container :: Bool
    , divided :: Direction
    , doubling :: Bool
    , inverted :: Bool
    , padded :: Direction
    , relaxed :: Relaxed
    , reversed :: Direction
    , stackable :: Bool
    , stretched :: Bool
    , textAlign :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default Grid where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Grid :: Grid -> Grid
pattern Grid g = g

instance Pure Grid where
    view Grid_ {..} =
        let
            cs =
                [ centered # "centered"
                , container # "container"
                , doubling # "doubling"
                , inverted # "inverted"
                , stackable # "stackable"
                , stretched # "stretched"
                , directionClass divided  "divided"
                , directionClass padded   "padded"
                , directionClass reversed "reversed"
                , celledClass celled
                , relaxedClass relaxed
                , textAlign
                , verticalAlign
                , widthProp columns "column" True
                ]
        in
            as (features & Classes cs & Theme GridT) children

instance HasProp As Grid where
    type Prop As Grid = Features -> [View] -> View
    getProp _ = as
    setProp _ a g = g { as = a }

instance HasFeatures Grid where
    getFeatures = features
    setFeatures as g = g { features = as }

instance HasChildren Grid where
    getChildren = children
    setChildren cs g = g { children = cs }

instance HasProp Celled Grid where
    type Prop Celled Grid = Maybe Txt
    getProp _ = celled
    setProp _ c g = g { celled = c }

instance HasProp Centered Grid where
    type Prop Centered Grid = Bool
    getProp _ = centered
    setProp _ c g = g { centered = c }

instance HasProp Columns Grid where
    type Prop Columns Grid = Txt
    getProp _ = columns
    setProp _ c g = g { columns = c }

instance HasProp IsContainer Grid where
    type Prop IsContainer Grid = Bool
    getProp _ = container
    setProp _ ic g = g { container = ic }

instance HasProp Divided Grid where
    type Prop Divided Grid = Maybe Txt
    getProp _ = divided
    setProp _ d g = g { divided = d }

instance HasProp Doubling Grid where
    type Prop Doubling Grid = Bool
    getProp _ = doubling
    setProp _ d g = g { doubling = d }

instance HasProp Inverted Grid where
    type Prop Inverted Grid = Bool
    getProp _ = inverted
    setProp _ i g = g { inverted = i }

instance HasProp Padded Grid where
    type Prop Padded Grid = Maybe Txt
    getProp _ = padded
    setProp _ p g = g { padded = p }

instance HasProp Relaxed Grid where
    type Prop Relaxed Grid = Maybe Txt
    getProp _ = relaxed
    setProp _ r g = g { relaxed = r }

instance HasProp Reversed Grid where
    type Prop Reversed Grid = [Txt]
    getProp _ = reversed
    setProp _ r g = g { reversed = r }

instance HasProp Stackable Grid where
    type Prop Stackable Grid = Bool
    type Prop Stackable Grid = Bool
    getProp _ = stackable
    setProp _ s g = g { stackable = s }

instance HasProp Stretched Grid where
    type Prop Stretched Grid = Bool
    getProp _ = stretched
    setProp _ s g = g { stretched = s }

instance HasProp TextAlign Grid where
    type Prop TextAlign Grid = Txt
    getProp _ = textAlign
    setProp _ t g = g { textAlign = t }

instance HasProp VerticalAlign Grid where
    type Prop VerticalAlign Grid = Txt
    getProp _ = verticalAlign
    setProp _ v g = g { verticalAlign = v }

data Column = Column_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , color :: Txt
    , computer :: Txt
    , floated :: Txt
    , largeScreen :: Txt
    , mobile :: Txt
    , only :: [Txt]
    , stretched :: Bool
    , tablet :: Txt
    , textAlign :: Txt
    , verticalAlign :: Txt
    , widescreen :: Txt
    , width :: Txt
    } deriving (Generic)

instance Default Column where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Column :: Column -> Column
pattern Column gc = gc

instance Pure Column where
    view Column_ {..} =
        let
            cs =
                [ color
                , stretched # "stretched"
                , multiProp only "only"
                , textAlign
                , (floated /= mempty) # (floated <>> "floated")
                , verticalAlign
                , widthProp computer "wide computer" def
                , widthProp largeScreen "wide large-screen" def
                , widthProp mobile "wide mobile" def
                , widthProp widescreen "wide widescreen" def
                , widthProp width "wide" def
                , "column"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Column where
    type Prop As Column = Features -> [View] -> View
    getProp _ = as
    setProp _ a gc = gc { as = a }

instance HasFeatures Column where
    getFeatures = features
    setFeatures as gc = gc { features = as }

instance HasChildren Column where
    getChildren = children
    setChildren cs gc = gc { children = cs }

instance HasProp Color Column where
    type Prop Color Column = Txt
    getProp _ = color
    setProp _ c gc = gc { color = c }

instance HasProp OnComputer Column where
    type Prop OnComputer Column = Txt
    getProp _ = computer
    setProp _ c gc = gc { computer = c }

instance HasProp Floated Column where
    type Prop Floated Column = Txt
    getProp _ = floated
    setProp _ f gc = gc { floated = f }

instance HasProp OnLargeScreen Column where
    type Prop OnLargeScreen Column = Txt
    getProp _ = largeScreen
    setProp _ ls gc = gc { largeScreen = ls }

instance HasProp OnMobile Column where
    type Prop OnMobile Column = Txt
    getProp _ = mobile
    setProp _ m gc = gc { mobile = m }

instance HasProp Only Column where
    type Prop Only Column = [Txt]
    getProp _ = only
    setProp _ o gc = gc { only = o }

instance HasProp Stretched Column where
    type Prop Stretched Column = Bool
    getProp _ = stretched
    setProp _ s gc = gc { stretched = s }

instance HasProp OnTablet Column where
    type Prop OnTablet Column = Txt
    getProp _ = tablet
    setProp _ t gc = gc { tablet = t }

instance HasProp TextAlign Column where
    type Prop TextAlign Column = Txt
    getProp _ = textAlign
    setProp _ t gc = gc { textAlign = t }

instance HasProp VerticalAlign Column where
    type Prop VerticalAlign Column = Txt
    getProp _ = verticalAlign
    setProp _ v gc = gc { verticalAlign = v }

instance HasProp OnWidescreen Column where
    type Prop OnWidescreen Column = Txt
    getProp _ = widescreen
    setProp _ w gc = gc { widescreen = w }

instance HasProp Width Column where
    type Prop Width Column = Txt
    getProp _ = width
    setProp _ w gc = gc { width = w }

data Row = Row_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , centered :: Bool
    , color :: Txt
    , columns :: Txt
    , divided :: Bool
    , only :: [Txt]
    , reversed :: Maybe Reversed
    , stretched :: Bool
    , textAlign :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default Row where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Row :: Row -> Row
pattern Row gr = gr

instance Pure Row where
    view Row_ {..} =
        let
            cs =
                [ color
                , centered # "centered"
                , divided # "divided"
                , stretched # "stretched"
                , multiProp only "only"
                , maybe "" (<>> "reversed") reversed
                , textAlign
                , verticalAlign
                , widthProp columns "columns" True
                , "row"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Row where
    type Prop As Row = Features -> [View] -> View
    getProp _ = as
    setProp _ a gr = gr { as = a }

instance HasFeatures Row where
    getFeatures = features
    setFeatures as gr = gr { features = as }

instance HasChildren Row where
    getChildren = children
    setChildren cs gr = gr { children = cs }

instance HasProp Color Row where
    type Prop Color Row = Txt
    getProp _ = color
    setProp _ c gr = gr { color = c }

instance HasProp Columns Row where
    type Prop Columns Row = Txt
    getProp _ = columns
    setProp _ c gr = gr { columns = c }

instance HasProp Divided Row where
    type Prop Divided Row = Bool
    getProp _ = divided
    setProp _ d gr = gr { divided = d }

instance HasProp Only Row where
    type Prop Only Row = [Txt]
    getProp _ = only
    setProp _ o gr = gr { only = o }

instance HasProp Reversed Row where
    type Prop Reversed Row = [Txt]
    getProp _ = reversed
    setProp _ r gr = gr { reversed = r }

instance HasProp Stretched Row where
    type Prop Stretched Row = Bool
    getProp _ = stretched
    setProp _ s gr = gr { stretched = s }

instance HasProp TextAlign Row where
    type Prop TextAlign Row = Txt
    getProp _ = textAlign
    setProp _ t gr = gr { textAlign = t }

instance HasProp VerticalAlign Row where
    type Prop VerticalAlign Row = Txt
    getProp _ = verticalAlign
    setProp _ v gr = gr { verticalAlign = v }

