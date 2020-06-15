{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Word
  (Word8)

import Termonad.App (defaultMain)
import Termonad.Config
  ( FontConfig, FontSize(FontSizePoints), Option(Set)
  , ShowScrollbar(ShowScrollbarAlways), defaultConfigOptions, defaultFontConfig
  , defaultTMConfig, fontConfig, fontFamily, fontSize, options, showScrollbar
  , confirmExit, showMenu, cursorBlinkMode, CursorBlinkMode(CursorBlinkModeOff)
  )
import Termonad.Config.Colour
  ( AlphaColour, ColourConfig, addColourExtension, createColour
  , createColourExtension, cursorBgColour, cursorFgColour, defaultColourConfig
  , backgroundColour, foregroundColour
  , Palette(BasicPalette, ExtendedPalette), palette
  )

import Termonad.Config.Vec
  (Vec (EmptyVec, (:*)), N8)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- | This sets the color of the cursor in the terminal.
--
-- This uses the "Data.Colour" module to define a dark-red color.
-- There are many default colors defined in "Data.Colour.Names".
cursBgColour :: AlphaColour Double
cursBgColour = createColour 230 230 250

cursFgColour :: AlphaColour Double
cursFgColour = createColour 41 42 68

bgColour :: AlphaColour Double
bgColour = createColour 41 42 68

fgColour :: AlphaColour Double
fgColour = createColour 232 230 237

normalColours = fmap (uncurry3 createColour) $
                     (102, 102, 153)
                  :* (105, 192, 250)
                  :* (139, 253, 225)
                  :* (1  , 234, 192)
                  :* (193, 127, 248)
                  :* (255, 146, 205)
                  :* (244, 242, 249)
                  :* (255, 252, 168)
                  :* EmptyVec                     

brightColours = fmap (uncurry3 createColour) $
                     (18 , 19 , 30 )
                  :* (122, 165, 255)
                  :* (86 , 211, 194)
                  :* (4  , 219, 181)
                  :* (191, 156, 249)
                  :* (221, 119, 85 )
                  :* (228, 227, 233)
                  :* (242, 231, 183)
                  :* EmptyVec
           
rebeccaPalette :: Palette (AlphaColour Double)
rebeccaPalette =  ExtendedPalette normalColours brightColours

-- | This sets the colors used for the terminal.  We only specify the background
-- color of the cursor.
colConf :: ColourConfig (AlphaColour Double)
colConf =
  defaultColourConfig
    { cursorBgColour = Set cursBgColour
    , cursorFgColour = Set cursFgColour
    , backgroundColour = Set bgColour
    , foregroundColour = Set fgColour
    , palette = rebeccaPalette
    }

-- | This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "DejaVu Sans Mono"
    , fontSize = FontSizePoints 12
    }

main :: IO ()
main = do
  colExt <- createColourExtension colConf
  let termonadConf =
        defaultTMConfig
          { options =
              defaultConfigOptions
                { fontConfig = fontConf
                  -- Make sure the scrollbar is always visible.
                , showScrollbar = ShowScrollbarAlways
                , confirmExit = False
                , showMenu = False
                , cursorBlinkMode = CursorBlinkModeOff
                }
          } `addColourExtension` colExt
  defaultMain termonadConf
