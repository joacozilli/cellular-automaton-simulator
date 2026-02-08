module Eval where

import           Types
import           Utils
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SVector (fromList, unsafeFreeze)
import qualified Data.Vector.Storable.Mutable as MSVector
import qualified Data.Map.Strict as Map (empty)
import           Control.Monad.ST
import           Control.Monad.Except
import           Control.Monad.Trans.Class (lift)

import Control.Applicative()
import Control.Monad (liftM, ap)
   

-- monad used in eval
newtype M a = M { runM :: Env -> Either Error a}

instance Functor M where
    fmap = liftM

instance Applicative M where
    pure x = M (\_ -> Right x)
    (<*>) = ap

instance Monad M where
    return = pure
    M h >>= f = M (\env -> case h env of
                            Left err -> Left err
                            Right x -> runM (f x) env)

-- throw error
throw :: Error -> M a
throw err = M (\_ -> Left err) 

-- return enviroment
ask :: M Env
ask = M Right

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



-- global transition to go from one configuration to the next. Create mutable vector
-- and convert it to inmutable. Uses ExceptT monad transformer to combine ST and Either.
globalTransition :: Conf                -- current configuration
                 -> (Env -> RGBA)       -- converted transition rule
                 -> LitNeighbors        -- literal neighborhood of each cell
                 -> Frontier            -- type of frontier in simulation
                 -> RGBA                -- default color
                 -> Conf
globalTransition c@(confVec,n,m) func neighbors fr def =
    let newconf =  runST $ do
                        let len = n*m
                        new <- MSVector.unsafeNew len
                        let loop i | i == len = return ()
                                    | otherwise = do let env = Env { cell = i,
                                                                    envConf = c,
                                                                    envNeighbors = neighbors,
                                                                    envVars = Map.empty,
                                                                    envFrontier = fr,
                                                                    defaultColor = def
                                                                    }
                                                         x = func env
                                                     MSVector.unsafeWrite new i x
                                                     loop (i+1)
                        loop 0
                        SVector.unsafeFreeze new
    in (newconf,n,m)


-- evaluation of rule on an enviroment
eval :: Rule -> M RGBA
eval (State s) = evalState s
eval (If cond r1 r2) = do b <- evalBool cond
                          if b
                            then do eval r1
                            else do eval r2
     
------------------------------- evaluation of State, Int and Bool expressions --------------------------------
evalState :: Exp State -> M RGBA
evalState Self = do env <- ask
                    return (cellColor (envConf env) (cell env) (defaultColor env))

evalState (Neighbor k) = do env <- ask
                            let i = cell env
                                nei = envNeighbors env Vector.! i Vector.! (k-1)
                            return (cellColor (envConf env) nei (defaultColor env))
                            
--evalState (LitColor rgba) = return rgba


evalInt :: Exp Int -> M Int
evalInt (Const n) = return n

evalInt (Neighbors s) = do env <- ask
                           color <- evalState s
                           let config = envConf env
                               neiVec = envNeighbors env Vector.! cell env
                               def = defaultColor env
                               g k nei = if cellColor config nei def == color
                                            then k+1 else k
                            in return (Vector.foldl' g 0 neiVec)

evalInt (Opp a) = do a' <- evalInt a
                     return (-a')


evalBool :: Exp Bool -> M Bool
evalBool BTrue = return True
evalBool BFalse = return False

evalBool (And a b) = do a' <- evalBool a
                        b' <- evalBool b
                        return (a' && b')

evalBool (Or a b) = do a' <- evalBool a
                       b' <- evalBool b
                       return (a' || b')

evalBool (Not a) = do a' <- evalBool a
                      return (not a')

evalBool (Lt a b) = do a' <- evalInt a
                       b' <- evalInt b
                       return (a' < b')

evalBool (Le a b) = do a' <- evalInt a
                       b' <- evalInt b
                       return (a' <= b')

evalBool (Gt a b) = do a' <- evalInt a
                       b' <- evalInt b
                       return (a' > b')

evalBool (Ge a b) = do a' <- evalInt a
                       b' <- evalInt b
                       return (a' >= b')

evalBool (EqInt a b) = do a' <- evalInt a
                          b' <- evalInt b
                          return (a' == b')

evalBool (NeqInt a b) = do a' <- evalInt a
                           b' <- evalInt b
                           return (a' /= b')

evalBool (EqState a b) = do a' <- evalState a
                            b' <- evalState b
                            return (a' == b')

evalBool (NeqState a b) = do a' <- evalState a
                             b' <- evalState b
                             return (a' /= b')

evalBool (In s set) = do s' <- evalState s
                         set' <- aux set
                         return (foldl (\b x -> b || x == s') False set')
                         where aux [x] = do x' <- evalState x
                                            return [x']
                               aux (x:xs) = do x' <- evalState x
                                               xs' <- aux xs
                                               return (x':xs')
                               aux [] = undefined
--------------------------------------------------------------------------------------------------------------