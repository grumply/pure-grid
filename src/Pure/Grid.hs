module Pure.Grid
  ( module Properties
  , module Tools
  , Grid(..), pattern Grid
  , Column(..), pattern Column
  , Row(..), pattern Row
  ) where

import Pure hiding (color,textAlign,verticalAlign,width)

import GHC.Generics as G

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
        . fmap (\w -> Txt.replace "-" " " w <<>> key)
        . Txt.words
        . Txt.replace " vertically" "-vertically"
        . Txt.replace "lare screen" "large-screen"
        . Txt.unwords
        $ val

(<>>) x y =
  case (Txt.null x, Txt.null y) of
    (False,False) -> x <<>> y
    (False,_) -> x
    (_,False) -> y
    _ -> ""
(<<>) = (<>>)

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

data Grid = Grid_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , celled :: Maybe Txt
    , centered :: Bool
    , columns :: Txt
    , container :: Bool
    , divided :: Maybe Txt
    , doubling :: Bool
    , inverted :: Bool
    , padded :: Maybe Txt
    , relaxed :: Maybe Txt
    , reversed :: [Txt]
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
                , maybe "" (<>> "celled") celled
                , maybe "" (<>> "divided") divided
                , maybe "" (<>> "padded") padded
                , maybe "" (<>> "relaxed") relaxed
                , multiProp reversed "reversed"
                , textAlign
                , verticalAlign
                , widthProp columns "column" True
                , "grid"
                ]
        in
            as (features & Classes cs) children

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
                , widthProp largeScreen "wide large screen" def
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
    , reversed :: [Txt]
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
                , multiProp reversed "reversed"
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

