{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}

module UI where

import           Data.Maybe           (fromJust, isJust, isNothing)
import           Debug.Trace
import           Debug.Trace.Helpers
import           GHC.Generics
import           Prelude              hiding (Either (..), id, (.))
import qualified SFML.Graphics       as SFML
import qualified SFML.Window         as SFML
import           Control.Lens
import Control.Category
import Data.Default
import Data.Array

import qualified Color as Color
import Coord
import Symbol
import GameState
import Entity
import GameMap

data DisplayContext = DisplayContext {
  _wnd   :: SFML.RenderWindow,
  _fnt   :: SFML.Font,
  _clock :: SFML.Clock
}
makeLenses ''DisplayContext

initDisplay :: IO DisplayContext
initDisplay = do
  let ctxSettings = Just $ SFML.ContextSettings 24 8 0 1 2 [SFML.ContextDefault]
  wnd <- SFML.createRenderWindow (SFML.VideoMode 1200 800 32) "Umbral" [SFML.SFDefaultStyle, SFML.SFResize] ctxSettings
  SFML.setFramerateLimit wnd 60
  let fontPath = "Everson Mono.ttf"
  fnt <- SFML.err $ SFML.fontFromFile fontPath
  clk <- SFML.createClock
  SFML.display wnd
  let display = DisplayContext { _wnd = wnd, _fnt = fnt, _clock = clk}
  return display

endDisplay :: DisplayContext -> IO ()
endDisplay context = do
  SFML.destroy (context ^. wnd)
  SFML.destroy (context ^. fnt)
  return ()

handleResize :: (?context :: DisplayContext) => Int -> Int -> IO ()
handleResize w h = do
  SFML.setWindowSize (?context ^. wnd) (SFML.Vec2u (fromIntegral w) (fromIntegral h))
  view <- SFML.getDefaultView (?context ^. wnd)
  SFML.resetView view (SFML.FloatRect 0 0 (fromIntegral w) (fromIntegral h))
  SFML.setView (?context ^. wnd) view
  return ()

convertColorToSFML :: Color.Color -> SFML.Color
convertColorToSFML (Color.Color r g b) = SFML.Color r' g' b' 255 where
  r' = fromIntegral r
  g' = fromIntegral g
  b' = fromIntegral b

fontSize = 20 -- FIXME

screenSize :: (?context :: DisplayContext) => IO ScreenCoord
screenSize = do
  (SFML.Vec2u xsize ysize) <- SFML.getWindowSize (?context ^. wnd)
  return $ Coord (fromIntegral xsize) (fromIntegral ysize)

cellSize :: (?context :: DisplayContext) => ScreenCoord
cellSize = Coord (floor ((fromIntegral fontSize) * 0.75)) (floor ((fromIntegral fontSize) * 0.66))

screenSizeCells :: (?context :: DisplayContext) => IO Coord
screenSizeCells = do
  size <- screenSize
  let cellCount = size `Coord.quot` cellSize
  return cellCount

celltoScreen :: (?context :: DisplayContext) => Coord -> ScreenCoord
celltoScreen coord = flipOrder $ coord * cellSize

fromWorldToScreen :: (?context :: DisplayContext) => WorldCoord -> WorldCoord -> IO (ScreenCoord)
fromWorldToScreen playerCoord worldCoord = return $ celltoScreen worldCoord

putSymbol :: (?context :: DisplayContext) => Coord -> Symbol -> IO ()
putSymbol coord symbol = do
  let c = (symbol ^. baseColor)
      t = (symbol ^. glyph)
      Coord tx ty = coord
      v = SFML.Vec2f (fromIntegral tx) (fromInteger ty)
  txt <- SFML.err SFML.createText
  SFML.setTextStringU txt [t]
  SFML.setTextFont txt (?context ^. fnt)
  SFML.setTextCharacterSize txt fontSize
  SFML.setTextColor txt $ convertColorToSFML c
  SFML.setPosition txt v
  SFML.drawText (?context ^. wnd) txt (Just SFML.renderStates)
  SFML.destroy txt

putEntity :: (?context :: DisplayContext) => Entity -> IO ()
putEntity entity = putSymbol (celltoScreen (entity ^. position)) (entity ^. symbol)

convertfromCoord (Coord xc yc) = SFML.Vec2f (fromIntegral xc) (fromIntegral yc)

centerViewOn :: (?context :: DisplayContext) => Coord -> IO ()
centerViewOn coord = do
  view <- SFML.getDefaultView (?context ^. wnd)
  SFML.setViewCenter view $ convertfromCoord $ celltoScreen coord
  SFML.setView (?context ^. wnd) view
  return ()

render :: DisplayContext -> GameState -> VisibleMap -> IO ()
render display state visibleMap = let ?context = display in do
    SFML.clearRenderWindow (display ^. wnd) $ SFML.Color 0 0 0 255
    centerViewOn (state ^. playerPosition)
    let visible e = (visibleMap <!> (e ^. position)) || (isPlayerEntity e)
        visibleEntities = filter visible (levelEntities (state ^. currLevel))
    mapM_ putEntity $ visibleEntities
    SFML.display (display ^. wnd)

-----
data PlayerCommand  = Go Direction
                    | Pass
                    | Save
                    | Load
                    | Quit deriving (Show, Read, Eq, Generic)

getPlayerCommand :: DisplayContext -> IO (Maybe PlayerCommand)
getPlayerCommand display = let ?context = display in do
  evt <- SFML.pollEvent (?context ^. wnd)
  case evt of
    Nothing -> return Nothing
    Just SFML.SFEvtClosed -> return $ Just Quit
    Just SFML.SFEvtResized {SFML.width = w, SFML.height = h} -> do
      handleResize w h
      return Nothing
    Just kEvt@SFML.SFEvtKeyPressed{} -> return (playerCommandFromKey kEvt)
    Just _ -> return Nothing

playerCommandFromKey :: SFML.SFEvent -> Maybe PlayerCommand
playerCommandFromKey SFML.SFEvtKeyPressed {SFML.code = keyCode,
                                      SFML.alt = altK,
                                      SFML.ctrl = ctrlK,
                                      SFML.shift = shiftK,
                                      SFML.sys = sysK} = case (keyCode, shiftK) of
                                        (SFML.KeySpace, _) -> Just $ Pass
                                        (SFML.KeyUp, _) -> Just $ Go Up
                                        (SFML.KeyDown, _) -> Just $ Go Down
                                        (SFML.KeyLeft, _) -> Just $ Go Left
                                        (SFML.KeyRight, _) -> Just $ Go Right
                                        (SFML.KeyQ, False) -> Just Quit
                                        (SFML.KeyS, False) -> Just Save
                                        (SFML.KeyL, False) -> Just Load
                                        _ -> Nothing
