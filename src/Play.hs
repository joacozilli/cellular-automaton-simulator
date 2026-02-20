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



sideBar :: World -> Picture
sideBar w = let (_,n,m) = conf w
                s = drawScale w
                width = fromIntegral m * s
                height = fromIntegral n * s
                boxW = max (width * 0.1) 70
                boxH = height
                gentext = translate (width/2 + 5) (boxH/2 - 20) $ scale 0.1 0.1 $ color (greyN 0.75) (Text (show (instant w)))
                rect = translate (width/2 + boxW/2) 0 (Pictures [color (dark $ dark red) $ rectangleSolid boxW boxH,
                                                                rectangleWire boxW boxH])
             in Pictures [rect,gentext]


gridLines :: Int        -- number of rows in grid
          -> Int        -- number of columns in grid
          -> Float      -- drawing scale
          -> [Picture]
gridLines n m s = verticalLines ++ horizontalLines
  where
    w = fromIntegral m * s
    h = fromIntegral n * s
    verticalLines = [color black $ line [(x, -h/2), (x, h/2)] | i <- [0..m], let x = -w/2 + fromIntegral i * s]
    horizontalLines = [color black $ line [(-w/2, y), (w/2, y)] | j <- [0..n], let y = -h/2 + fromIntegral j * s]


drawAux :: World -> Picture
drawAux w = let (config,n,m) = conf w in bitmapOfByteString m n (BitmapFormat TopToBottom PxRGBA) (buildByteString config) False

-- Generate picture from a world.
draw :: World -> Picture
draw w = let (_,n,m) = conf w
             s = drawScale w
             grid = Pictures (scale s s (drawAux w) : gridLines n m s)
             (x,y) = translation w
          in Translate x y (Pictures [grid, sideBar w])


-- Update to next world.
update :: Float -> World -> World
update _ w = if paused w
              then w
              else let newconf = if par w
                                    then globalTransitionPAR (conf w) (transition w) (neighbors w) (frontier w) (defaultColor w)
                                    else globalTransitionSEQ (conf w) (transition w) (neighbors w) (frontier w) (defaultColor w)
                    in  w {conf = newconf, instant = instant w + 1, initial = False}


-- handle keyboard/mouse events
handleInput :: Event -> World -> World
-- pause simulation with spacebar
handleInput (EventKey (SpecialKey KeySpace) Down _ _) w = w {paused = not (paused w)}

-- restart simulation with r
handleInput (EventKey (Char 'r') Down _ _) w = let (_,n,m) = conf w
                                                in w {conf = initConf n m (defaultColor w),
                                                      instant = 0,
                                                      paused = True,
                                                      initial = True
                                                     }

-- go to next instant with n
handleInput (EventKey (Char 'n') Down _ _) w = if paused w
                                                then let w' = update 0 w { paused = False }
                                                      in w' { paused = True }
                                                else w

-- click on cell to change color
handleInput (EventKey (MouseButton LeftButton) Down _ position) w =
      if initial w
        then let (_,n,m) = conf w
              in case mouseToCell n m (translation w) position (drawScale w) of
                  Just x -> w {conf = colorCell x (nextColor (conf w) x (colors w)) (conf w)}
                  Nothing -> w
        else w      

-- zoom in/out with mouse wheel or arrows
handleInput (EventKey (SpecialKey KeyUp) Down _ _) w = w {drawScale = 1 + drawScale w}
handleInput (EventKey (MouseButton WheelUp) _ _ _) w = w {drawScale = 1 + drawScale w}
handleInput (EventKey (MouseButton WheelDown) _ _ _) w = w {drawScale = max (drawScale w - 1) 2}
handleInput (EventKey (SpecialKey KeyDown) Down _ _) w = w {drawScale = max (drawScale w - 1) 2}   

-- move around with WASD
handleInput (EventKey (Char 'a') Down _ _) w = let (x,y) = translation w
                                                   (_,n,_) = conf w
                                                   width = fromIntegral n
                                                in w {translation = (x + max (drawScale w * width*0.1) 100 / drawScale w, y)}
handleInput (EventKey (Char 'd') Down _ _) w = let (x,y) = translation w
                                                   (_,n,_) = conf w
                                                   width = fromIntegral n
                                                in w {translation = (x - max (drawScale w * width*0.1) 100 / drawScale w, y)}
handleInput (EventKey (Char 'w') Down _ _) w = let (x,y) = translation w 
                                                   (_,_,m) = conf w
                                                   height = fromIntegral m                       
                                                in w {translation = (x, y - max (drawScale w * height*0.1) 100 / drawScale w)}
handleInput (EventKey (Char 's') Down _ _) w = let (x,y) = translation w 
                                                   (_,_,m) = conf w
                                                   height = fromIntegral m                   
                                                in w {translation = (x, y + max (drawScale w * height*0.1) 100 / drawScale w)}

-- recenter with c
handleInput (EventKey (Char 'c') Down _ _) w = w {translation = (0,0)}

handleInput _ w = w



-- Initial configuration (all cells default color).
initConf :: Int -> Int -> RGBA -> Conf
initConf n m def = let c = [ def | _ <- [0..(n*m)-1]]
                    in (SVector.fromListN (n*m) c, n, m)

-- Initial world prior to starting simulation.
initWorld :: Automata 
          -> Frontier
          -> (Env -> RGBA)  -- converted transition rule
          -> Int            -- number of rows in grid
          -> Int            -- number of columns in grid
          -> Bool           -- world's par flag
          -> World
initWorld (CA _ sm nv _ def) fr f n m b = let defcolor = fromJust $ Map.lookup def sm
                                          in World { transition = f,
                                                      conf = initConf n m defcolor,
                                                      neighbors = computeNeighbors n m nv fr,
                                                      colors = Map.elems sm,
                                                      defaultColor = defcolor,
                                                      frontier = fr,
                                                      paused = True,
                                                      initial = True,
                                                      instant = 0,
                                                      drawScale = 5,
                                                      translation = (0,0),
                                                      par = b
                                                   }





