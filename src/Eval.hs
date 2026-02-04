module Eval where

import           Types
import           Utils
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SVector (fromList, unsafeFreeze)
import qualified Data.Vector.Storable.Mutable as MSVector
import           Control.Monad.ST
import           Control.Monad.Except
import           Control.Monad.Trans.Class (lift)

import Control.Applicative()
import Control.Monad (liftM, ap)


-- Enviroment for the evaluation of transition rule in a cell
type Env = (Coord,          -- coordinate of cell
            Conf,           -- configuration 
            Neighborhood,   -- neighborhood vector
            Frontier,       -- type of frontier of simulation
            RGBA            -- default color
           )            
         

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

-- compute literal neighborhood of all cell. For each cell, the literal neighborhood
-- is a vector of indexes, considering -1 as out of range neighbor in a Default frontier
-- simulation.
neighbors :: Int            -- number of rows in grid
          -> Int            -- number of columns in grid
          -> Neighborhood   -- neighborhood vector
          -> Vector.Vector (Vector.Vector Int) 
neighbors n m v = Vector.generate (n*m) $ \k ->
                    Vector.generate (Vector.length v) $ \i ->
                        let cell = bidim k m
                            nei = cell + (v Vector.! i)
                        in case 



-- global transition to go from one configuration to the next. Create mutable vector
-- and convert it to inmutable. Uses ExceptT monad transformer to combine ST and Either.
globalTransition :: Conf                -- current configuration
                 -> Rule                -- converted transition rule AST
                 -> Neighborhood        -- neighborhood vector
                 -> Frontier            -- type of frontier in simulation
                 -> RGBA                -- default color
                 -> Either Error Conf
globalTransition c@(confVec,n,m) rule neighbors fr def =
    let newconf =  runST $ runExceptT $ do
                            let len = n*m
                            new <- lift $ MSVector.unsafeNew len
                            let loop i | i == len = return ()
                                       | otherwise = do x <- ExceptT (pure (runM (eval rule) (bidim i m, c, neighbors, fr, def)))
                                                        lift $ MSVector.unsafeWrite new i x
                                                        loop (i+1)
                            loop 0
                            lift $ SVector.unsafeFreeze new
    in case newconf of
        Left err -> Left err
        Right newvec -> Right (newvec,n,m)



-- evaluation of rule on an enviroment
eval :: Rule -> M RGBA
eval (State s) = evalState s
eval (If cond r1 r2) = do b <- evalBool cond
                          if b
                            then do eval r1
                            else do eval r2
     
------------------------------- evaluation of State, Int and Bool expressions --------------------------------
evalState :: Exp State -> M RGBA
evalState Self = do (cell,config,_,fr,def) <- ask
                    return (cellColor config cell fr def)

evalState (Neighbor k) = do (cell,config,neighbors,fr,def) <- ask
                            let (i,j) = cell + (neighbors Vector.! (k-1)) 
                             in return (cellColor config (i,j) fr def)
                            
evalState (LitColor rgba) = return rgba


evalInt :: Exp Int -> M Int
evalInt (Const n) = return n

evalInt (Neighbors s) = do rgba <- evalState s
                           (cell,config,neighbors,fr,def) <- ask
                           let g k nei = let (i,j) = cell + nei
                                             color = cellColor config (i,j) fr def
                                          in if color == rgba then k+1 else k
                            in return (Vector.foldl' g 0 neighbors)    
                                     
evalInt (Sum a b) = do a' <- evalInt a
                       b' <- evalInt b
                       return (a' + b')
evalInt (Subs a b) = do a' <- evalInt a
                        b' <- evalInt b
                        return (a' - b')

evalInt (Prod a b) = do a' <- evalInt a
                        b' <- evalInt b
                        return (a' * b')

evalInt (Div a b) = do a' <- evalInt a
                       b' <- evalInt b
                       if b' == 0
                        then throw ZeroDiv
                        else return (a' `div` b')

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