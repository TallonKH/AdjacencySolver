module RandState where

import System.Random

newtype RandState a = RandState {
    runRandState :: StdGen -> (a, StdGen)
}

instance Functor RandState where
    fmap f ma = RandState $ \gen ->
        let (val, gen') = runRandState ma gen
        in (f val, gen')

instance Applicative RandState where
    pure a = RandState $ \gen -> (a, gen)
    rf <*> rv = RandState $ \gen ->
        let (f, gen')  = runRandState rf gen
            (v, gen'') = runRandState rv gen'
        in (f v, gen'')

instance Monad RandState where
    ra >>= f = RandState $ \gen ->
        let (a, gen')  = runRandState ra gen
            (b, gen'') = runRandState (f a) gen'
        in (b, gen'')

evalRand :: RandState a -> StdGen -> a
evalRand (RandState f) s = fst $ f s