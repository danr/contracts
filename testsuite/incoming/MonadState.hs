{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module MonadState where

import Contracts
import Prelude (String,error)

infixr 9 .
f . g = \x -> f (g x)

infixr 0 $

f $ x = f x

fst (a,b) = a
snd (a,b) = b

infixl 1 >>=
infixl 1 >>

class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    m >> n = m >>= \_ -> n

    fail :: String -> m a

-- | Identity functor and monad.
newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)
    fail     = error

-- | Minimal definition is either both of @get@ and @put@ or just @state@
class Monad m => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s

    -- | Replace the state inside the monad.
    put :: s -> m ()

-- ---------------------------------------------------------------------------
-- | A state transformer monad parameterized by:
--
--   * @s@ - The state.
--
--   * @m@ - The inner monad.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runStateT' ('mapStateT' f m) = f . 'runStateT' m@
mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

-- | @'withStateT' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withStateT' f m = 'modify' f >> m@
withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= k  = StateT $ \s ->
        runStateT m s >>= \ ~(a, s') ->
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance Monad m => MonadState s (StateT s m) where
     -- | Fetch the current value of the state within the monad.
     get = StateT $ \s -> return (s, s)

     -- | @'put' s@ sets the state within the monad to @s@.
     put s = StateT $ \_ -> return ((), s)

-- | @'modify' f@ is an action that updates the state to the result of
-- applying @f@ to the current state.
modify :: MonadState s m => (s -> s) -> m ()
modify f = get >>= put . f

-- | Get a specific component of the state, using a projection function
-- supplied.
--
-- * @'gets' f = 'liftM' f 'get'@
gets :: MonadState s m => (s -> a) -> m a
gets f = get >>= return . f

newtype State s a = State { unState :: StateT s Identity a }
  deriving (Monad,MonadState s)

runState :: s -> State s a -> (a,s)
runState s (State m) = runIdentity (runStateT m s)

runState_cf = runState ::: CF --> CF --> CF

gets_cf     = (gets :: forall s a . (s -> a) -> State s a)
                ::: (CF --> CF) --> CF
