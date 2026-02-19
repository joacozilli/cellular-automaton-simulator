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

defaultOptions :: Options
defaultOptions = Options { optGridRows = 100, optGridColumns = 100,
                           optFrontier = Default, optSpeed = 10, optParallelism = True }
                    

-- Options descriptions.
options :: [OptDescr (Either String (Options -> Options))]
options = [Option ['r'] ["rows"]
                  (ReqArg (\n -> case readMaybe n of
                                    Just x | x > 0 -> Right (\opts -> opts {optGridRows = max 10 (min x 1000)})
                                    Just _ -> Left "rows must be positive integer"
                                    Nothing -> Left $ "invalid rows value: "++n)
                               "INT")
                  "set number of rows of grid (default: 100)",

           Option ['c'] ["columns"]
                  (ReqArg (\n -> case readMaybe n of
                                    Just x | x > 0 -> Right (\opts -> opts {optGridColumns = max 10 (min x 1000)})
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
                                  Just x | x > 0 -> Right (\opts -> opts {optSpeed = max 1 (min x 30)})
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


-- Read and prepare options. Exit program if there is an error.
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




 
-- Print error message for given error.
printError :: Error -> IO ()
printError (UndefState name) = putStrLn ("  - Undefined state \""++name++"\" referenced.")
printError (NeighborOutOfRange k) = putStrLn ("  - trying to access invalid/out of range neighbor with nei("++show k++")")
printError (UndefVar name) = putStrLn ("  - undefined variable \""++name++"\" referenced.")
printError (SameColor ss) = putStrLn ("  - " ++ aux ss ++ " share same color.")
                            where
                                aux [s1,s2] = show s1 ++ " and " ++ show s2
                                aux (x:xs) = show x ++ ", " ++ aux xs
                                aux _ = undefined

                                          
-- Print all errors, avoiding duplicates.
printErrors :: [Error] -> IO ()
printErrors = aux []
       where
              aux _ [] = return ()
              aux xs (e:es) = if e `elem` xs
                               then aux xs es
                               else do printError e
                                       aux (e:xs) es

-- Check for defined states that share color.
checkStates :: States -> [Error]
checkStates sm = let xs = Map.toList sm
                     xs' = map (\(k,v) -> (v,[k])) xs
                     newmap = Map.fromListWith (++) xs'
                     shareColors = Map.toList newmap
                     noSingletons = filter (\(_,l) -> case l of
                                                        [_] -> False
                                                        _ -> True) shareColors
                 in map (\(_,l) -> SameColor l) noSingletons




startSimulation :: Automata -> Options -> IO ()
startSimulation ca@(CA name states neigh rule def) opts =
       let n = Vector.length neigh
           Out (res,errs1) = conversion states n Map.empty rule
           errs2 = checkStates states
           err = case Map.lookup def states of
                     Nothing -> [UndefState def]
                     Just _ -> []
           errors = err ++ errs2 ++ errs1
       in if not (null errors) 
              then do putStrLn "[ERROR] following errors where detected in automata definition:"
                      printErrors errors
                      exitFailure
              else do print rule
                      play
                        (displayWindow name)
                        white
                        (optSpeed opts)
                        (initWorld ca (optFrontier opts) res (optGridRows opts) (optGridColumns opts))
                        draw
                        handleInput
                        update

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

displayWindow :: String -> Display
displayWindow name = InWindow name (1000,1000) (0,0)