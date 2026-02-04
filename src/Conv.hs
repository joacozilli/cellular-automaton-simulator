module Conv where

import Types
import qualified Data.Map.Strict as Map

import Control.Applicative()
import Control.Monad (liftM, ap)


-- monad to be used in conversion.
newtype Output e a = Out (a,e)

instance Monoid e => Functor (Output e) where
    fmap = liftM

instance Monoid e => Applicative (Output e) where
    pure x = Out (x, mempty)
    (<*>) = ap

instance Monoid e => Monad (Output e) where
    return = pure
    Out (x,e1) >>= f = let Out (y,e2) = f x in Out (y, mappend e1 e2)

-- write an error to the monad
write :: e -> Output e ()
write err = Out ((),err)

{-
Convert all occasions of Lit STATE_NAME to LitColor STATE_COLOR and check
for undefined state and undefined neighbor errors. The convertion is successful
if no errors are encountered.
-}
conversion :: States                -- map of states
            -> Int                  -- size of neighborhood
            -> Rule                 -- transition rule
            -> Output [Error] Rule  -- return all encountered errors or converted transition rule
conversion sm n (If b r1 r2) = do b' <- convBool sm n b
                                  r1' <- conversion sm n r1
                                  r2' <- conversion sm n r2
                                  return (If b' r1' r2')
conversion sm n (State s) = do s' <- convState sm n s
                               return (State s')


----------------------------- auxiliar functions for conversion -----------------------------------------
convState :: States -> Int -> Exp State -> Output [Error] (Exp State)
convState sm _ (Lit name) = case Map.lookup name sm of
                                Just rgba -> return (LitColor rgba)
                                Nothing -> write [UndefState name] >> return (Lit name)
convState _ n t@(Neighbor k) = if k <= n then return t
                                         else write [NeighborOutOfRange k] >> return t
convState _ _ s = return s

convInt :: States -> Int -> Exp Int -> Output [Error] (Exp Int)
convInt sm n (Const k) = return (Const k)
convInt sm n (Neighbors state) = do state' <- convState sm n state
                                    return (Neighbors state')
convInt sm n (Sum a b) = do a' <- convInt sm n a
                            b' <- convInt sm n b
                            return (Sum a' b')
convInt sm n (Subs a b) = do a' <- convInt sm n a
                             b' <- convInt sm n b
                             return (Subs a' b')
convInt sm n (Prod a b) = do a' <- convInt sm n a
                             b' <- convInt sm n b
                             return (Prod a' b')
convInt sm n (Div a b) = do a' <- convInt sm n a
                            b' <- convInt sm n b
                            return (Div a' b')
convInt sm n (Opp a) = do a' <- convInt sm n a
                          return (Opp a')

convBool :: States -> Int -> Exp Bool -> Output [Error] (Exp Bool)
convBool sm n (And a b) = do a' <- convBool sm n a
                             b' <- convBool sm n b
                             return (And a' b')

convBool sm n (Or a b) = do a' <- convBool sm n a
                            b' <- convBool sm n b
                            return (Or a' b')

convBool sm n (Lt a b) = do a' <- convInt sm n a
                            b' <- convInt sm n b
                            return (Lt a' b')

convBool sm n (Le a b) = do a' <- convInt sm n a
                            b' <- convInt sm n b
                            return (Le a' b')

convBool sm n (Gt a b) = do a' <- convInt sm n a
                            b' <- convInt sm n b
                            return (Gt a' b')

convBool sm n (Ge a b) = do a' <- convInt sm n a
                            b' <- convInt sm n b
                            return (Ge a' b')

convBool sm n (EqInt a b) = do a' <- convInt sm n a
                               b' <- convInt sm n b
                               return (EqInt a' b')

convBool sm n (NeqInt a b) = do a' <- convInt sm n a
                                b' <- convInt sm n b
                                return (NeqInt a' b')

convBool sm n (EqState a b) = do a' <- convState sm n a
                                 b' <- convState sm n b
                                 return (EqState a' b')

convBool sm n (NeqState a b) = do a' <- convState sm n a
                                  b' <- convState sm n b
                                  return (NeqState a' b')

convBool sm n (In s set) = do s' <- convState sm n s
                              set' <- aux set
                              return (In s' set')
                                where aux [x] = do x' <- convState sm n x
                                                   return [x']
                                      aux (x:xs) = do x' <- convState sm n x
                                                      xs' <- aux xs
                                                      return (x':xs')
                                      aux [] = undefined