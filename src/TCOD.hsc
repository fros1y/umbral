{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}

module TCOD where

import Foreign
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.C.Types

#include "libtcod.h"

data TCODMapRaw
data TCODDijkstraRaw

newtype TCODMap = TCODMap (ForeignPtr TCODMapRaw)
newtype Dijkstra = Dijkstra (ForeignPtr TCODDijkstraRaw)

type CBool = CInt
boolToCInt :: Bool -> CInt
boolToCInt True = 1
boolToCInt False = 0

cIntToBool :: CInt -> Bool
cIntToBool 0 = False
cIntToBool _ = True

foreign import ccall unsafe "TCOD_map_new" tcod_map_new :: CInt -> CInt -> IO (Ptr TCODMapRaw)
foreign import ccall unsafe "TCOD_map_set_properties" tcod_map_set_properties :: Ptr TCODMapRaw -> CInt -> CInt -> CBool -> CBool -> IO ()
foreign import ccall unsafe "TCOD_map_compute_fov" tcod_map_compute_fov :: Ptr TCODMapRaw -> CInt -> CInt -> CInt -> CBool -> CInt -> IO ()
foreign import ccall unsafe "TCOD_map_is_in_fov" tcod_map_is_in_fov :: Ptr TCODMapRaw -> CInt -> CInt -> IO CBool

newMap :: Int -> Int -> IO TCODMap
newMap xSize' ySize' = do
  let xSize = fromIntegral xSize'
      ySize = fromIntegral ySize'
  mapRaw <- tcod_map_new xSize ySize
  mapForeign <- newForeignPtr_ mapRaw
  return $ TCODMap mapForeign

setGrid :: TCODMap -> Int -> Int -> Bool -> Bool -> IO ()
setGrid (TCODMap mapRaw) cellX' cellY' transparent' walkable' = withForeignPtr mapRaw $ \ptr -> do
  let cellX = fromIntegral cellX'
      cellY = fromIntegral cellY'
      transparent = boolToCInt transparent'
      walkable = boolToCInt walkable'
  tcod_map_set_properties ptr cellX cellY transparent walkable

computeFOVFrom :: TCODMap -> Int -> Int -> Int -> Bool -> IO ()
computeFOVFrom (TCODMap mapRaw) playerX' playerY' maxRadius' lightWalls' = withForeignPtr mapRaw $ \ptr -> do
  let playerX = fromIntegral playerX'
      playerY = fromIntegral playerY'
      maxRadius = fromIntegral maxRadius'
      lightWalls = boolToCInt lightWalls'
  tcod_map_compute_fov ptr playerX playerY maxRadius lightWalls (#const FOV_BASIC)

inFOV :: TCODMap -> Int -> Int -> IO Bool
inFOV (TCODMap mapRaw) cellX' cellY' = withForeignPtr mapRaw $ \ptr -> do
  let cellX = fromIntegral cellX'
      cellY = fromIntegral cellY'
  result <- tcod_map_is_in_fov ptr cellX cellY
  return $ cIntToBool result

foreign import ccall unsafe "TCOD_dijkstra_new" tcod_dijkstra_new :: Ptr TCODMapRaw -> CFloat -> IO (Ptr TCODDijkstraRaw)
foreign import ccall unsafe "&TCOD_dijkstra_delete" tcod_dijkstra_delete :: FunPtr (Ptr TCODDijkstraRaw -> IO ())
foreign import ccall unsafe "TCOD_dijkstra_compute" tcod_dijkstra_compute :: Ptr TCODDijkstraRaw -> CInt -> CInt -> IO ()
foreign import ccall unsafe "TCOD_dijkstra_path_set" tcod_dijkstra_path_set :: Ptr TCODDijkstraRaw -> CInt -> CInt -> IO CBool
foreign import ccall unsafe "TCOD_dijkstra_size" tcod_dijkstra_size :: Ptr TCODDijkstraRaw -> IO CInt
foreign import ccall unsafe "TCOD_dijkstra_get" tcod_dijkstra_get :: Ptr TCODDijkstraRaw -> CInt -> Ptr CInt -> Ptr CInt -> IO ()

floatToCFloat :: Float -> CFloat
floatToCFloat float = fromRational (toRational float)  -- FIXME: This makes me cry

newDijkstra :: TCODMap -> Float -> IO Dijkstra
newDijkstra (TCODMap mapRaw) cost' = withForeignPtr mapRaw $ \ptr -> do
  let cost = floatToCFloat cost'
  dijkstraRaw <- tcod_dijkstra_new ptr cost
  dijkstraForeign <- newForeignPtr tcod_dijkstra_delete dijkstraRaw
  return $ Dijkstra dijkstraForeign

computeDijkstraFrom :: Dijkstra -> Int -> Int -> IO ()
computeDijkstraFrom (Dijkstra raw) cellX' cellY' = withForeignPtr raw $ \ptr -> do
  let cellX = fromIntegral cellX'
      cellY = fromIntegral cellY'
  tcod_dijkstra_compute ptr cellX cellY

setDijkstraPathTo :: Dijkstra -> Int -> Int -> IO Bool
setDijkstraPathTo (Dijkstra raw) cellX' cellY' = withForeignPtr raw $ \ptr -> do
  let cellX = fromIntegral cellX'
      cellY = fromIntegral cellY'
  walkable <- tcod_dijkstra_path_set ptr cellX cellY
  return $ cIntToBool walkable

dijkstraPathLength :: Dijkstra -> IO Int
dijkstraPathLength (Dijkstra raw) = withForeignPtr raw $ \ptr -> do
  size <- tcod_dijkstra_size ptr
  return $ fromIntegral size

getDijkstraStep :: Dijkstra -> Int -> IO (Int, Int)
getDijkstraStep (Dijkstra raw) step' =
  withForeignPtr raw $ \ptr ->
  alloca $ \xptr ->
  alloca $ \yptr -> do
    let step = fromIntegral step
    tcod_dijkstra_get ptr step xptr yptr
    xval <- peek xptr
    yval <- peek yptr
    return (fromIntegral xval, fromIntegral yval)
