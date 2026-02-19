module Eval where

import           Types
import           Utils
import qualified Data.Vector as Vector (generate, length, (!))
import qualified Data.Vector.Storable as SVector (generate, concat)
import qualified Data.Map.Strict as Map (empty)

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
                                    let c = bidim k m
                                        x = c + (v Vector.! l)
                                    in if validIndex x n m
                                        then unidim x m
                                        else case fr of
                                                Default -> -1
                                                Toroidal -> unidim (toroidCell x n m) m



-- global transition to go from one configuration to the next, computing each cell sequentially.
globalTransitionSEQ :: Conf                -- current configuration
                    -> (Env -> RGBA)       -- converted transition rule
                    -> LitNeighbors        -- literal neighborhood of each cell
                    -> Frontier            -- type of frontier in simulation
                    -> RGBA                -- default color
                    -> Conf
globalTransitionSEQ c@(_,n,m) func neighs fr def =
    let newconf = SVector.generate (n*m) (\i -> let env = Env { cell = i,
                                                                envConf = c,
                                                                envNeighbors = neighs,
                                                                envVars = Map.empty,
                                                                envFrontier = fr,
                                                                envDefaultColor = def
                                                                }
                                                in func env)
    in (newconf,n,m)
                         

-- The same as globalTransition, but compute cells in parallel.
globalTransitionPAR :: Conf
                    -> (Env -> RGBA)
                    -> LitNeighbors
                    -> Frontier
                    -> RGBA
                    -> Conf
globalTransitionPAR c@(_,n,m) func neighs fr def =
                    let len = n*m
                        chunkSize = div len numCapabilities
                        chunkRanges size limit = go 0 where go i | i >= limit = []
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