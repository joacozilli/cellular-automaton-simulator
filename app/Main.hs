module Main (main) where

import Parser
import Types
import Conv
import Eval
import Play
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game

rule :: Automata -> Rule
rule (CA _ _ _ r _) = r

disp :: Display
disp = InWindow "grid" (1000,1000) (0,0)

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do content <- readFile file
                         case parse content 1 1 of
                                Failed err -> putStrLn err
                                Ok result -> do putStrLn "Parsing successful!"
                                                print result
                                                checkRule result
                                  

printError :: Error -> IO ()
printError (UndefState name) = putStrLn ("  - undefined state \""++name++"\" referenced in transition rule")
printError (NeighborOutOfRange k) = putStrLn ("  - trying to access invalid/out of range neighbor with nei("++show k++") in transition rule")

checkRule :: Automata -> IO ()
checkRule (CA name states neigh rule def) = let n = Vector.length neigh
                                            in case conversion states n rule of
                                                Out (res,[]) -> do print res
                                                                   play disp white 20
                                                                      (initWorld (CA name states neigh res def) Default 100 100)
                                                                      draw
                                                                      handleInput
                                                                      update
                                                Out (res,errors) -> do putStrLn "[ERROR] following errors where detected in automata definition:"
                                                                       aux errors
                                                                       print res
                                          where
                                            aux [e] = printError e
                                            aux (e:es) = do printError e
                                                            aux es