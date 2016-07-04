{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}

module TCOD where

import Foreign.Ptr
import Foreign.ForeignPtr

#include "libtcod.h"

data TCODMapRaw

newtype TCODMap = TCODMap (ForeignPtr TCODMapRaw)

foreign import ccall unsafe "TCOD_map_new" tcod_map_new :: Int -> Int -> IO (Ptr TCODMapRaw)
foreign import ccall unsafe "TCOD_map_set_properties" tcod_map_set_properties :: Ptr TCODMapRaw -> Int -> Int -> Bool -> Bool -> IO ()
foreign import ccall unsafe "TCOD_map_compute_fov" tcod_map_compute_fov :: Ptr TCODMapRaw -> Int -> Int -> Int -> Bool -> Int -> IO ()
foreign import ccall unsafe "TCOD_map_is_in_fov" tcod_map_is_in_fov :: Ptr TCODMapRaw -> Int -> Int -> IO Bool

newMap :: Int -> Int -> IO TCODMap
newMap xSize ySize = do
  mapRaw <- tcod_map_new xSize ySize
  mapForeign <- newForeignPtr_ mapRaw
  return $ TCODMap mapForeign

setGrid :: TCODMap -> Int -> Int -> Bool -> Bool -> IO ()
setGrid (TCODMap mapRaw) cellX cellY transparent walkable = withForeignPtr mapRaw $ \ptr -> do
  tcod_map_set_properties ptr cellX cellY transparent walkable

computeFOVFrom :: TCODMap -> Int -> Int -> Int -> Bool -> IO ()
computeFOVFrom (TCODMap mapRaw) playerX playerY maxRadius lightWalls = withForeignPtr mapRaw $ \ptr -> do
  tcod_map_compute_fov ptr playerX playerY maxRadius lightWalls (#const FOV_BASIC)

inFOV :: TCODMap -> Int -> Int -> IO Bool
inFOV (TCODMap mapRaw) cellX cellY = withForeignPtr mapRaw $ \ptr -> do
  tcod_map_is_in_fov ptr cellX cellY
