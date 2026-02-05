module Utils where

import           Types
import           Data.Word (Word8)
import           Graphics.Gloss.Data.Color
import           Data.Bits ((.|.), shiftL, shiftR, (.&.))
import           GHC.ByteOrder (targetByteOrder, ByteOrder(..))
import qualified Data.Vector.Storable as SVector


-- Pack 4 word8 representing a color in rgba format to a single word32 (RGBA type).
-- Endianness is considered so byte order is always [r,g,b,a].
packToRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> RGBA
packToRGBA r g b a = case targetByteOrder of
                        LittleEndian -> fromIntegral r
                                        .|. shiftL (fromIntegral g) 8
                                        .|. shiftL (fromIntegral b) 16
                                        .|. shiftL (fromIntegral a) 24
                        BigEndian -> fromIntegral a
                                     .|. shiftL (fromIntegral b) 8
                                     .|. shiftL (fromIntegral g) 16
                                     .|. shiftL (fromIntegral r) 24

-- convert a gloss color to RGBA
colorToRGBA :: Color -> RGBA
colorToRGBA c = let (r,g,b,a) = rgbaOfColor c
                in packToRGBA (f r) (f g) (f b) (f a)
                    where f = floor . (*255)



-- convert RGBA (packed Word32) back to a gloss color
rgbaToColor :: RGBA -> Color
rgbaToColor rgba =
  let f = (/255) . fromIntegral
  in case targetByteOrder of
       LittleEndian ->
         let r = f (rgba .&. 0xFF)
             g = f ((rgba `shiftR` 8) .&. 0xFF)
             b = f ((rgba `shiftR` 16) .&. 0xFF)
             a = f ((rgba `shiftR` 24) .&. 0xFF)
         in makeColor r g b a
       BigEndian ->
         let a = f (rgba .&. 0xFF)
             b = f ((rgba `shiftR` 8) .&. 0xFF)
             g = f ((rgba `shiftR` 16) .&. 0xFF)
             r = f ((rgba `shiftR` 24) .&. 0xFF)
         in makeColor r g b a


-- convert coordinate to unidimensional representation
-- The coordinate is assumed to be in range.
unidim :: Coord -- coordinate
        -> Int  -- number of columns
        -> Int
unidim (i,j) m = i*m + j

-- convert index of unidimensional vector to coordinate.
-- The index is assumed to be in range.
bidim :: Int -- index
      -> Int -- number of columns
      -> Coord
bidim k m = (k `div` m, k `mod` m)

-- verify if given coordinate is in range of grid.
validIndex :: Coord
           -> Int   -- number of rows 
           -> Int   -- number of columns
           -> Bool
validIndex (i,j) n m = i >= 0 && i <= n-1
                    && j >= 0 && j <= m-1


-- return corresponding color of a cell in a configuration
cellColor :: Conf    -- configuration
          -> Int     -- cell index
          -> RGBA    -- default color
          -> RGBA
cellColor (vector,_,_) idx def = if idx == -1
                                    then def
                                    else vector SVector.! idx




------------------------------------ color cell on click --------------------------------------------

-- convert mouse position to correspondig cell. Return Nothing if mouse
-- is outside grid.
mouseToCell :: Int             -- number of rows of grid
            -> Int             -- number of columns of grid
            -> (Float, Float)  -- mouse position in gloss coordinate system
            -> Float           -- drawing scale
            -> Maybe Coord
mouseToCell n m (x,y) s = let h = fromIntegral n * s
                              w = fromIntegral m * s
                              inGrid = x >= (-w/2) && x < (w/2) && y >= (-h/2) && y < (h/2)
                          in if inGrid
                             then let (i,j) =  (floor ( h/2 - y - 1), floor (x + w/2))
                                   in Just (i `div` floor s, j `div` floor s)
                             else Nothing

-- color cell in grid. Used for mouse event.
colorCell :: Coord -> RGBA -> Conf -> Conf
colorCell cell color (confvec,n,m) = let list = SVector.toList confvec
                                         idx = unidim cell m
                                     in (SVector.fromListN (n*m) (aux 0 idx color list),n,m)
            where
                aux k limit c (x:xs) | k < limit = x : aux (k+1) limit c xs
                                     | k == limit = c:xs