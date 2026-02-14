module Main (main) where

import           Parser
import           Types
import           Conv
import           Play
import           System.Environment (getArgs)
import           System.Console.GetOpt
import           System.Exit (exitFailure)
import           Text.Read (readMaybe)
import           Control.Monad (foldM)

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import           Graphics.Gloss.Data.Display
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Interface.Pure.Game



data Options = Options { optGridRows :: Int,
                         optGridColumns :: Int,
                         optFrontier :: Frontier,
                         optSpeed :: Int,
                         optParallelism :: Bool
                        }

-- Options descriptions.
options :: [OptDescr (Either String (Options -> Options))]
options = [Option ['r'] ["rows"]
                  (ReqArg (\n -> case readMaybe n of
                                    Just x | x > 0 -> Right (\opts -> opts {optGridRows = x})
                                    Just _ -> Left "rows must be positive integer"
                                    Nothing -> Left $ "invalid rows value: "++n)
                               "INT")
                  "set number of rows of grid (default: 100)",

           Option ['c'] ["columns"]
                  (ReqArg (\n -> case readMaybe n of
                                    Just x | x > 0 -> Right (\opts -> opts {optGridColumns = x})
                                    Just _ -> Left "columns must be positive integer"
                                    Nothing -> Left $ "invalid columns value: "++n)
                               "INT")
                  "set number of columns of grid (default: 100)",

           Option ['f'] ["frontier"]
                  (ReqArg (\fr -> case fr of
                                    "toroidal" -> Right (\opts -> opts {optFrontier = Toroidal})
                                    "default" -> Right (\opts -> opts {optFrontier = Default})
                                    _ -> Left $ "invalid frontier value: "++fr)
                                "toroidal | default")
                  "specify type of frontier in simulation (default: default)",
 
           Option ['s'] ["speed"]
                  (ReqArg (\s -> case readMaybe s of
                                  Just x | x > 0 -> Right (\opts -> opts {optSpeed = x})
                                  Just _  -> Left "speed must be positive integer"
                                  Nothing -> Left $ "invalid speed value: "++s)
                                "INT")
                  "specify steps for second in simulation (default: 10)",

           Option ['g'] ["gobal-transition"]
                  (ReqArg (\val -> case val of
                                    "seq" -> Right (\opts -> opts {optParallelism = False})
                                    "par" -> Right (\opts -> opts {optParallelism = True})
                                    _ -> Left $ "invalid global-transition value: "++val)
                          "seq | par")      
                  "specify if global transition computes configurations sequencially or in parallel (default: par)"
            ]



defaultOptions :: Options
defaultOptions = Options { optGridRows = 100, optGridColumns = 100,
                           optFrontier = Default, optSpeed = 10, optParallelism = True }


-- foldM :: (foldable t, monad m) => (b -> a -> m b) -> b -> t a -> m b
-- foldM :: (Options -> Either String (Options -> Options) -> Either String Options)
--       -> Options
--       -> [Either String (Options -> Options)]
--       -> Either String Options


parseOptions :: [String] -> IO Options
parseOptions args = case getOpt Permute options args of
                     (_,_,errs) | not (null errs) -> do mapM_ putStrLn errs
                                                        exitFailure
                     (opts,_,_) -> do let result = foldM applyOption defaultOptions opts
                                      case result of
                                          Left err -> do putStrLn $ "Error: " ++ err
                                                         exitFailure
                                          Right finalOpts -> return finalOpts
                            where
                            applyOption :: Options -> Either String (Options -> Options) -> Either String Options
                            applyOption op (Right f) = return (f op)
                            applyOption _ (Left err) = Left err



displayWindow :: Display
displayWindow = InWindow "CAsim" (1000,1000) (0,0)

 
-- Print error message for given error type in transition rule.
printTransitionError :: Error -> IO ()
printTransitionError (UndefState name) = putStrLn ("  - undefined state \""++name++"\"")
printTransitionError (NeighborOutOfRange k) = putStrLn ("  - trying to access invalid/out of range neighbor with nei("++show k++")")
printTransitionError (UndefVar name) = putStrLn ("  - undefined variable \""++name++"\"")
                                          
                     
startSimulation :: Automata -> Options -> IO ()
startSimulation ca@(CA _ states neigh rule def) opts =
       let n = Vector.length neigh
       in case conversion states n Map.empty rule of
              Out (_,errors) | not (null errors) -> do putStrLn  "[ERROR] following errors where detected in transition rule:"
                                                       aux errors
                                                       exitFailure
              Out (res,_) -> case Map.lookup def states of
                                Nothing -> putStrLn ("[ERROR] undefined state \""++def++"\"in Default field")
                                Just _ -> play displayWindow
                                               white
                                               (optSpeed opts)
                                               (initWorld ca (optFrontier opts) res (optGridRows opts) (optGridColumns opts))
                                               draw
                                               handleInput
                                               update                                          
       where
       aux [e] = printTransitionError e
       aux (e:es) = do printTransitionError e
                       aux es
       aux [] = undefined

main :: IO ()
main = do args <- getArgs
          case args of
            (file:rest) -> do opts <- parseOptions rest
                              content <- readFile file
                              case parse content 1 1 of
                                   Failed err -> do putStrLn err
                                                    exitFailure
                                   Ok result -> startSimulation result opts
            _ -> return ()
