module Eval where

import           Types
import           Utils
import qualified Data.Vector as Vector (generate, length, (!))
import qualified Data.Vector.Storable as SVector (unsafeFreeze, generate, concat)
import qualified Data.Vector.Storable.Mutable as MSVector (unsafeNew, unsafeWrite)
import qualified Data.Map.Strict as Map (empty)
import           Control.Monad.ST

import           Control.Parallel.Strategies
import           GHC.Conc (numCapabilities)

-- compute literal neighborhood of all cells. For each cell, the literal neighborhood
-- is a vector of indexes to access neighbors directly, considering -1 as out of range
-- neighbor in a Default frontier simulation.
computeNeighbors :: Int            -- number of rows in grid
                 -> Int            -- number of columns in grid
                 -> Neighborhood   -- neighborhood vector
                 -> Frontier       -- type of frontier in simulation
                 -> LitNeighbors
computeNeighbors n m v fr = Vector.generate (n*m) $ \k ->
                                Vector.generate (Vector.length v) $ \l ->
                                    let cell = bidim k m
                                        (i,j) = cell + (v Vector.! l)
                                    in if validIndex (i,j) n m
                                        then unidim (i,j) m
                                        else case fr of
                                                Default -> -1
                                                Toroidal -> unidim (toroidCell (i,j) n m) m



-- global transition to go from one configuration to the next, computing each cell sequentially.
-- It creates mutable vector and converts it to inmutable using the ST monad.
globalTransition :: Conf                -- current configuration
                 -> (Env -> RGBA)       -- converted transition rule
                 -> LitNeighbors        -- literal neighborhood of each cell
                 -> Frontier            -- type of frontier in simulation
                 -> RGBA                -- default color
                 -> Conf
globalTransition c@(_,n,m) func neighs fr def =
    let newconf =  runST $ do
                        let len = n*m
                        new <- MSVector.unsafeNew len
                        let loop i | i == len = return ()
                                   | otherwise = do let env = Env { cell = i,
                                                                    envConf = c,
                                                                    envNeighbors = neighs,
                                                                    envVars = Map.empty,
                                                                    envFrontier = fr,
                                                                    envDefaultColor = def
                                                                    }
                                                        x = func env
                                                    MSVector.unsafeWrite new i x
                                                    loop (i+1)
                        loop 0
                        SVector.unsafeFreeze new
    in (newconf,n,m)

-- The same as globalTransition, but compute cells parallelly
globalTransitionPAR :: Conf
                    -> (Env -> RGBA)
                    -> LitNeighbors
                    -> Frontier
                    -> RGBA
                    -> Conf
globalTransitionPAR c@(_,n,m) func neighs fr def =
                    let len = n*m
                        chunkSize = div len numCapabilities
                        chunkRanges size len = go 0 where go i | i >= len = []
                                                               | otherwise =
                                                                let j = min (i+size-1) (len-1)
                                                                in (i,j) : go (j+1)
                        ranges = chunkRanges chunkSize len

                        buildChunks (start,end) = SVector.generate (end-start+1) (\i ->
                                                            let env = Env { cell = i + start,
                                                                            envConf = c,
                                                                            envNeighbors = neighs,
                                                                            envVars = Map.empty,
                                                                            envFrontier = fr,
                                                                            envDefaultColor = def
                                                                            }
                                                                in func env)                     
                        chunksComputed = parMap rdeepseq buildChunks ranges
                    in (SVector.concat chunksComputed, n, m)