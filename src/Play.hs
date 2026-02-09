module Play where

import                  Types
import                  Eval
import                  Conv ()
import                  Utils
import                  Graphics.Gloss.Data.Bitmap
import                  Graphics.Gloss.Interface.Pure.Game
import qualified        Data.Vector.Storable as SVector
import                  Data.Word ()
import qualified        Data.Map.Strict as Map
import                  Data.Maybe


-- draw a legend with state color boxes and labels below the grid, and generation info
drawLegend :: World -> Picture
drawLegend w =
  let (_, n, m) = conf w           -- n = width (cols), m = height (rows)
      s = drawScale w
      items = Map.toList (states w)
      wpx = fromIntegral n * s
      hpx = fromIntegral m * s
      y = -hpx / 2 - 30            -- legend y position (below grid)
      boxW = 20
      boxH = 20
      spacing = 10
      startX = -wpx / 2 + 10
      statePics = zipWith (\(name,rgba) idx ->
                  translate (startX + fromIntegral idx * (boxW + spacing)) y $
                    Pictures [ color (rgbaToColor rgba) $ rectangleSolid boxW boxH
                             , translate (boxW/2 + 5) 0 $ scale 0.18 0.18 $ Text name
                             ]) items [0..]
      genPic = translate ( -wpx/2 + 10 ) (y - 20) $ scale 0.2 0.2 $ Text ("gen: " ++ show (instant w))
  in Pictures (statePics ++ [genPic])

gridLines :: Int -> Int -> Float -> [Picture]
gridLines n m s = verticalLines ++ horizontalLines
  where
    w = fromIntegral n * s
    h = fromIntegral m * s
    verticalLines = [color black $ line [(x, -h / 2), (x,  h / 2)] | i <- [0 .. n], let x = -w / 2 + fromIntegral i * s]
    horizontalLines = [color black $ line [(-w / 2, y), ( w / 2, y)] | j <- [0 .. m], let y = -h / 2 + fromIntegral j * s]


drawAux :: World -> Picture
drawAux w = let (config,n,m) = conf w in bitmapOfByteString n m (BitmapFormat TopToBottom PxRGBA) (buildByteString config) False

-- generate picture from a world and scale it to cell size
draw :: World -> Picture
draw w = if initial w || True
            then let (_,n,m) = conf w
                     s = drawScale w
                     grid = Pictures (scale s s (drawAux w) : gridLines n m s)
                     legend = drawLegend w
                  in Pictures [grid, legend]
            else scale (drawScale w) (drawScale w) $ drawAux w



update :: Float -> World -> World
update _ w = if paused w
              then w
              else let newconf = globalTransitionPAR (conf w) (transition w) (neighbors w) (frontier w) (defaultColor w)
                    in  w {conf = newconf, instant = instant w + 1, initial = False}



handleInput :: Event -> World -> World
-- pause simulation with spacebar
handleInput (EventKey (SpecialKey KeySpace) Down _ _) w = w {paused = not (paused w)}

-- restart simulation with r
handleInput (EventKey (Char 'r') Down _ _) w = let (_,n,m) = conf w
                                                in (initWorld (automata w) (frontier w) (transition w) n m) {drawScale = drawScale w}

-- go to next instant with n
handleInput (EventKey (Char 'n') Down _ _) w = if paused w && not (initial w)
                                                then let w' = update 0 w { paused = False }
                                                      in w' { paused = True }
                                                else w

-- click on cell to change color
handleInput (EventKey (MouseButton LeftButton) Down _ position) w =
      if initial w
        then let (_,n,m) = conf w
              in case mouseToCell n m position (drawScale w) of
                  Just cell -> w {conf = colorCell cell (colorToRGBA black) (conf w)}
                  Nothing -> w
        else w
        
-- zoom in/out with mouse wheel or arrows
handleInput (EventKey (SpecialKey KeyUp) Down _ _) w = w {drawScale = 1 + drawScale w}
handleInput (EventKey (MouseButton WheelUp) _ _ _) w = w {drawScale = 1 + drawScale w}
handleInput (EventKey (MouseButton WheelDown) _ _ _) w = w {drawScale = max (drawScale w - 1) 2}
handleInput (EventKey (SpecialKey KeyDown) Down _ _) w = w {drawScale = max (drawScale w - 1) 2}                                             
handleInput _ w = w



f :: Int -> RGBA
f _ = colorToRGBA white


initConf :: Int -> Int -> Conf
initConf n m = let c = [f k | k <- [0..(n*m)-1]]
                in (SVector.fromListN (n*m) c, n, m)

-- initial world prior to starting simulation
initWorld :: Automata -> Frontier -> (Env -> RGBA) -> Int -> Int -> World
initWorld ca@(CA _ sm nv _ def) fr f n m = World { transition = f,
                                                   conf = initConf n m,
                                                   neighbors = computeNeighbors n m nv fr,
                                                   states = sm,
                                                   defaultColor = fromJust $ Map.lookup def sm,
                                                   frontier = fr,
                                                   paused = True,
                                                   initial = True,
                                                   instant = 0,
                                                   drawScale = 5,
                                                   speed = 1.0
                                                 }





