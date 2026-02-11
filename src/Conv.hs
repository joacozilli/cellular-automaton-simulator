module Conv where

{-# HLINT ingore "use const" #-}

import           Types
import           Utils
import qualified Data.Map.Strict as Map (insert, lookup)
import qualified Data.Vector as Vector (foldl', (!))
import qualified Data.Vector.Storable as SVector
import           Data.Maybe (fromJust)

import           Control.Applicative()
import           Control.Monad (liftM, ap)


-- monad to be used in conversion; it allows to keep track of all
-- encountered errors troughout the conversion.
newtype Output e a = Out (a,e)

instance Monoid e => Functor (Output e) where
    fmap = liftM

instance Monoid e => Applicative (Output e) where
    pure x = Out (x, mempty)
    (<*>) = ap

instance Monoid e => Monad (Output e) where
    return = pure
    Out (x,e1) >>= f = let Out (y,e2) = f x in Out (y, mappend e1 e2)

-- write an error
write :: e -> Output e ()
write err = Out ((),err)


-- Convert rule AST to a pure haskell function of type Env -> RGBA. Also check for
-- undefined state, neighbor and variable errors. The conversion is successful if
-- no errors are encountered.
conversion :: States                -- map of states
            -> Int                  -- size of neighborhood
            -> Vars                 -- map of variables
            -> Rule                 -- transition rule
            -> Output [Error] (Env -> RGBA)
conversion sm n _ (State s) = convState sm n s

conversion sm n vm (If b r1 r2) = do prop <- convBool sm n vm b
                                     f1 <- conversion sm n vm r1
                                     f2 <- conversion sm n vm r2
                                     return (\env -> if prop env then f1 env else f2 env)

conversion sm n vm (Let x exp rule) = do let vm' = Map.insert x 0 vm
                                         fexp <- convInt sm n vm exp
                                         frule <- conversion sm n vm' rule
                                         return (\env -> let n = fexp env
                                                             env' = env {envVars = Map.insert x n (envVars env)}
                                                           in frule env')

----------------------------- auxiliar functions for conversion -----------------------------------------

convState :: States -> Int -> Exp State -> Output [Error] (Env -> RGBA)
convState sm _ (Lit name) = case Map.lookup name sm of
                                Just rgba -> return (\_ -> rgba)
                                Nothing -> write [UndefState name] >> return (\_ -> 0)

convState _ n t@(Neighbor k) = if k <= n
                                    then return (\env ->
                                            let i = cell env
                                                nei = envNeighbors env Vector.! i Vector.! (k-1)
                                            in cellColor (envConf env) nei (envDefaultColor env))
                                    else write [NeighborOutOfRange k] >> return (\_ -> 0)

convState _ _ self = return (\env -> cellColor (envConf env) (cell env) (envDefaultColor env))


convInt :: States -> Int -> Vars -> Exp Int -> Output [Error] (Env -> Int)
convInt _ _ _ (Const n) = return (\_ -> n)

convInt _ _ vm (Var x) = case Map.lookup x vm of
                            Just _ -> return (\env -> fromJust $ Map.lookup x (envVars env) )
                            Nothing ->  write [UndefVar x] >> return (\_ -> 0)

convInt sm n _ (Neighbors state) = do f <- convState sm n state
                                      return (\env -> 
                                                let color = f env
                                                    config = envConf env
                                                    neiVec = envNeighbors env Vector.! cell env
                                                    def = envDefaultColor env
                                                    g k nei = if cellColor config nei def == color
                                                                then k+1 else k
                                                in Vector.foldl' g 0 neiVec
                                             )
convInt sm n vm (Opp a) = do f <- convInt sm n vm a
                             return (\env -> -(f env))


convBool :: States -> Int -> Vars -> Exp Bool -> Output [Error] (Env -> Bool)
convBool sm n vm (And a b) = do fa <- convBool sm n vm a
                                fb <- convBool sm n vm b
                                return (\env -> fa env && fb env)
convBool sm n vm (Or a b) = do fa <- convBool sm n vm a
                               fb <- convBool sm n vm b
                               return (\env -> fa env || fb env)

convBool sm n vm (Lt a b) = do fa <- convInt sm n vm a
                               fb <- convInt sm n vm b
                               return (\env -> fa env < fb env)

convBool sm n vm (Le a b) = do fa <- convInt sm n vm a
                               fb <- convInt sm n vm b
                               return (\env -> fa env <= fb env)

convBool sm n vm (Gt a b) = do fa <- convInt sm n vm a
                               fb <- convInt sm n vm b
                               return (\env -> fa env > fb env)

convBool sm n vm (Ge a b) = do fa <- convInt sm n vm a
                               fb <- convInt sm n vm b
                               return (\env -> fa env >= fb env)

convBool sm n vm (EqInt a b) = do fa <- convInt sm n vm a
                                  fb <- convInt sm n vm b
                                  return (\env -> fa env == fb env)

convBool sm n vm (NeqInt a b) = do fa <- convInt sm n vm a
                                   fb <- convInt sm n vm b
                                   return (\env -> fa env /= fb env)

convBool sm n _ (EqState a b) = do fa <- convState sm n a
                                   fb <- convState sm n b
                                   return (\env -> fa env == fb env)

convBool sm n _ (NeqState a b) = do fa <- convState sm n a
                                    fb <- convState sm n b
                                    return (\env -> fa env /= fb env)

convBool sm n _ (In s set) = do f <- convState sm n s
                                g <- aux set
                                return (\env -> let rgba = f env
                                                    xs = g env
                                                in foldl (\b x -> b || x == rgba) False xs)
                                    where aux [x] = do f <- convState sm n x
                                                       return (\env -> [f env])
                                          aux (x:xs) = do f <- convState sm n x
                                                          g <- aux xs
                                                          return (\env -> f env : g env)
                                          aux [] = undefined
