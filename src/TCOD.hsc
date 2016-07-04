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

foreign import ccall unsafe "TCOD_dijkstra_new" tcod_dijkstra_new :: Ptr TCODMapRaw -> Float -> IO (Ptr TCODDijkstraRaw)
foreign import ccall unsafe "&TCOD_dijkstra_delete" tcod_dijkstra_delete :: FunPtr (Ptr TCODDijkstraRaw -> IO ())
foreign import ccall unsafe "TCOD_dijkstra_compute" tcod_dijkstra_compute :: Ptr TCODDijkstraRaw -> Int -> Int -> IO ()
foreign import ccall unsafe "TCOD_dijkstra_path_set" tcod_dijkstra_path_set :: Ptr TCODDijkstraRaw -> Int -> Int -> IO Bool
foreign import ccall unsafe "TCOD_dijkstra_size" tcod_dijkstra_size :: Ptr TCODDijkstraRaw -> IO Int
foreign import ccall unsafe "TCOD_dijkstra_get" tcod_dijkstra_get :: Ptr TCODDijkstraRaw -> Int -> Ptr CInt -> Ptr CInt -> IO ()

newDijkstra :: TCODMap -> Float -> IO Dijkstra
newDijkstra (TCODMap mapRaw) cost = withForeignPtr mapRaw $ \ptr -> do
  dijkstraRaw <- tcod_dijkstra_new ptr cost
  dijkstraForeign <- newForeignPtr tcod_dijkstra_delete dijkstraRaw
  return $ Dijkstra dijkstraForeign

computeDijkstraFrom :: Dijkstra -> Int -> Int -> IO ()
computeDijkstraFrom (Dijkstra raw) cellX cellY = withForeignPtr raw $ \ptr -> do
  tcod_dijkstra_compute ptr cellX cellY

setDijkstraPathTo :: Dijkstra -> Int -> Int -> IO Bool
setDijkstraPathTo (Dijkstra raw) cellX cellY = withForeignPtr raw $ \ptr -> do
  walkable <- tcod_dijkstra_path_set ptr cellX cellY
  return walkable

dijkstraPathLength :: Dijkstra -> IO Int
dijkstraPathLength (Dijkstra raw) = withForeignPtr raw $ \ptr -> do
  size <- tcod_dijkstra_size ptr
  return size

getDijkstraStep :: Dijkstra -> Int -> IO (Int, Int)
getDijkstraStep (Dijkstra raw) step =
  withForeignPtr raw $ \ptr ->
  alloca $ \xptr ->
  alloca $ \yptr -> do
    tcod_dijkstra_get ptr step xptr yptr
    xval <- peek xptr
    yval <- peek yptr
    return (fromIntegral xval, fromIntegral yval)
