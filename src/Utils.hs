module Utils where

import                  Types
import                  Data.Word (Word8)
import                  Graphics.Gloss.Data.Color
import                  Data.Bits ((.|.), shiftL, shiftR, (.&.))
import                  GHC.ByteOrder (targetByteOrder, ByteOrder(..))
import qualified        Data.Vector.Storable as SVector
import qualified        Data.Vector.Storable.Mutable as MSVector (unsafeNew, unsafeWrite)
import qualified        Data.ByteString as B
import qualified        Data.ByteString.Internal as BI (fromForeignPtr)
import                  Control.Monad.ST

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

-- Convert a gloss color to RGBA.
colorToRGBA :: Color -> RGBA
colorToRGBA c = let (r,g,b,a) = rgbaOfColor c
                in packToRGBA (f r) (f g) (f b) (f a)
                    where f = floor . (*255)



-- Convert RGBA to gloss color.
rgbaToColor :: RGBA -> Color
rgbaToColor rgba =
  let f = (/255) . fromIntegral
  in case targetByteOrder of
       LittleEndian ->
         let r = f (rgba .&. 0xFF)
             g = f (shiftR rgba 8 .&. 0xFF)
             b = f (shiftR rgba 16 .&. 0xFF)
             a = f (shiftR rgba 24 .&. 0xFF)
         in makeColor r g b a
       BigEndian ->
         let a = f (rgba .&. 0xFF)
             b = f (shiftR rgba 8 .&. 0xFF)
             g = f (shiftR rgba 16 .&. 0xFF)
             r = f (shiftR rgba 24 .&. 0xFF)
         in makeColor r g b a


-- Convert coordinate to unidimensional representation.
-- The coordinate is assumed to be in range.
unidim :: Coord -- coordinate
        -> Int  -- number of columns
        -> Int
unidim (Coord (i,j)) m = i*m + j

-- Convert index of unidimensional vector to coordinate.
-- The index is assumed to be in range.
bidim :: Int -- index
      -> Int -- number of columns
      -> Coord
bidim k m = Coord (k `div` m, k `mod` m)

-- Verify if given coordinate is in range of grid.
validIndex :: Coord
           -> Int   -- number of rows 
           -> Int   -- number of columns
           -> Bool
validIndex (Coord (i,j)) n m = i >= 0 && i <= n-1
                            && j >= 0 && j <= m-1


-- Return corresponding color of a cell in a configuration.
cellColor :: Conf    -- configuration
          -> Int     -- cell index
          -> RGBA    -- default color
          -> RGBA
cellColor (vector,_,_) idx def = if idx == -1
                                    then def
                                    else vector SVector.! idx

-- Given a coord and size of grid, return corresponding cell in grid
-- of such coord with a toroidal frontier.
toroidCell :: Coord -> Int -> Int -> Coord
toroidCell (Coord (i,j)) n m = Coord (a,b) where
                            a | i >= n = i-n
                              | i < 0 = n+i
                              | otherwise = i

                            b | j >= m = j-m
                              | j < 0 = m+j
                              | otherwise = j
    
-- Convert vector of RGBA to a bytestring.
buildByteString :: SVector.Vector RGBA -> B.ByteString
buildByteString v = let v8 :: SVector.Vector Word8
                        v8 = SVector.unsafeCast v
                        (ptr,off,len) = SVector.unsafeToForeignPtr v8
                    in BI.fromForeignPtr ptr off len


------------------------------------ color cell on click --------------------------------------------

-- Convert mouse position to correspondig cell. Return Nothing if mouse
-- is outside grid.
mouseToCell :: Int             -- number of rows of grid
            -> Int             -- number of columns of grid
            -> (Float, Float)  -- world translation
            -> (Float, Float)  -- mouse position in gloss coordinate system
            -> Float           -- drawing scale
            -> Maybe Coord
mouseToCell n m (tx,ty) (x,y) s = let h = fromIntegral n * s
                                      w = fromIntegral m * s
                                      (xx,yy) = (x - tx, y - ty)
                                      inGrid = xx >= -w/2 && xx < w/2 && yy >= -h/2 && yy < h/2
                                  in if inGrid
                                    then let (i,j) =  (floor ( h/2 - yy - 1), floor (xx + w/2))
                                          in Just $ Coord (i `div` floor s, j `div` floor s)
                                    else Nothing

-- Color cell in grid. Used for mouse event.
colorCell :: Coord -> RGBA -> Conf -> Conf
colorCell c color (confvec,n,m) =
    let newconf = runST $ do
                          let len = n*m
                              idx = unidim c m
                          new <- MSVector.unsafeNew len
                          let loop i | i == len = return ()
                                     | i == idx = do MSVector.unsafeWrite new i color
                                                     loop (i+1)
                                     | otherwise = do MSVector.unsafeWrite new i (confvec SVector.! i)
                                                      loop (i+1)
                          loop 0
                          SVector.unsafeFreeze new
    in (newconf,n,m)