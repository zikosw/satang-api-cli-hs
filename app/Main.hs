{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void)

import Data.Aeson
import Data.Aeson.TH
import Data.Scientific
import qualified Data.Map.Strict as Map
import Network.HTTP.Simple


import Graphics.Vty
import Brick.AttrMap
import Brick.Main
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Types (Widget)
import Brick.Util (fg, on)

data MarketCap = MarketCap {
      last :: String
    , baseVolume  :: String
    , quoteVolume  :: String
    , percentChange  :: String
    } deriving (Show)
$(deriveJSON defaultOptions ''MarketCap)

toSci ::String -> Scientific
toSci str = read str :: Scientific

toFixed ::Scientific -> String
toFixed = formatScientific Fixed (Just 2)


toRow (pair,mkc) = priceStyle row
    where
        row = center (str pair)
            <+> vBorder <+>
            center ((toDec2 . percentChange) mkc)
            <+> vBorder <+>
            center (( toDec2 . Main.last) mkc)
            <+> vBorder <+>
            center ((toDec2 . baseVolume) mkc)
            <+> vBorder <+>
            center ((toDec2 . quoteVolume) mkc)

        toDec2 = str . toFixed . toSci

        priceStyle
            | pc < 0 = withAttr redAttr
            | pc > 0 = withAttr greenAttr
            | otherwise = id
            where
                pc = ( toSci . percentChange) mkc

drawUI :: Map.Map String MarketCap -> [Widget ()]
drawUI mktCaps = [ui]
    where
        ui = withBorderStyle unicode $
            borderWithLabel (str "MarketCap") $
            vBox $ header:rows
        header = withAttr hdAttr $
            center (str "Pair") <+> vBorder <+>
            center (str "%") <+> vBorder <+>
            center (str "Price") <+> vBorder <+>
            center (str "Vol.") <+> vBorder <+>
            center (str "Total")
        rows = map toRow $ Map.toList mktCaps

hdAttr :: AttrName
hdAttr = attrName "header"

redAttr :: AttrName
redAttr = attrName "price:down"

greenAttr :: AttrName
greenAttr = attrName "price:up"

theMap :: AttrMap
theMap = attrMap defAttr [
    (hdAttr,    withStyle (fg black) bold),
    (redAttr,    withStyle (fg red) bold),
    (greenAttr,  withStyle (fg cyan) bold)
    ]

theApp :: App (Map.Map String MarketCap) e ()
theApp =
    App { appDraw = drawUI
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = do
    response <- httpJSON "http://api.tdax.com/marketcap"
    let mktCaps = getResponseBody response :: Map.Map String MarketCap
    void $ defaultMain theApp mktCaps