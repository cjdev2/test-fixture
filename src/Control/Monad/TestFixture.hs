module Control.Monad.TestFixture
  ( module Control.Monad.RWS.Class
  , WST
  , WS
  , TestFixtureT
  , TestFixture
  , unTestFixtureT
  , logTestFixtureT
  , evalTestFixtureT
  , execTestFixtureT
  , runTestFixtureT
  , unTestFixture
  , logTestFixture
  , evalTestFixture
  , execTestFixture
  , runTestFixture
  , arg0
  , arg1
  , arg2
  , arg3
  , arg4
  , arg5
  , arg6
  , arg7
  , unimplemented
  ) where

import Control.Monad.RWS
import Control.Monad.RWS.Class
import Data.Functor.Identity
import Control.Monad.Reader

type WST w s m = RWST () w s m
type WS w s = WST w s Identity
type TestFixtureT r w s m = ReaderT (r (WST w s m)) (WST w s m)
type TestFixture r w s = TestFixtureT r w s Identity

unTestFixtureT :: Monad m => TestFixtureT r () s m a -> r (WST () s m)  -> s -> m a
unTestFixtureT stack env st = fmap fst (evalTestFixtureT stack env st)

logTestFixtureT :: Monad m => TestFixtureT r w s m a -> r (WST w s m)  -> s -> m w
logTestFixtureT stack env st = fmap snd (evalTestFixtureT stack env st)

evalTestFixtureT :: Monad m => TestFixtureT r w s m a -> r (WST w s m)  -> s -> m (a, w)
evalTestFixtureT stack env st = evalRWST (runReaderT stack env) () st

execTestFixtureT :: Monad m => TestFixtureT r w s m a -> r (WST w s m) -> s -> m (s, w)
execTestFixtureT stack env st = execRWST (runReaderT stack env) () st

runTestFixtureT :: Monad m => TestFixtureT r w s m a -> r (WST w s m)  -> s -> m (a, s, w)
runTestFixtureT stack env st = runRWST (runReaderT stack env) () st

unTestFixture :: TestFixture r () s a -> r (WS () s)  -> s -> a
unTestFixture stack env st = runIdentity (unTestFixtureT stack env st)

logTestFixture :: TestFixture r w s a -> r (WS w s)  -> s -> w
logTestFixture stack env st = runIdentity (logTestFixtureT stack env st)

evalTestFixture :: TestFixture r w s a -> r (WS w s)  -> s -> (a, w)
evalTestFixture stack env st = runIdentity (evalTestFixtureT stack env st)

execTestFixture :: TestFixture r w s a -> r (WS w s)  -> s -> (s, w)
execTestFixture stack env st = runIdentity (execTestFixtureT stack env st)

runTestFixture :: TestFixture r w s a -> r (WS w s)  -> s -> (a, s, w)
runTestFixture stack env st = runIdentity (runTestFixtureT stack env st)

arg0 :: (Monoid w) => (r (WS w s) -> WS w s a) -> TestFixture r w s a
arg0 rec = asks rec >>= lift

arg1 :: (Monoid w) => (r (WS w s) -> a -> WS w s b) -> a -> TestFixture r w s b
arg1 rec a = do
  fn <- asks rec
  lift $ fn a

arg2 :: (Monoid w) => (r (WS w s) -> a -> b -> WS w s c) -> a -> b -> TestFixture r w s c
arg2 rec a b = do
  fn <- asks rec
  lift $ fn a b

arg3 :: (Monoid w) => (r (WS w s) -> a -> b -> c -> WS w s d) -> a -> b -> c -> TestFixture r w s d
arg3 rec a b c = do
  fn <- asks rec
  lift $ fn a b c

arg4 :: (Monoid w) => (r (WS w s) -> a -> b -> c -> d -> WS w s e) -> a -> b -> c -> d -> TestFixture r w s e
arg4 rec a b c d = do
  fn <- asks rec
  lift $ fn a b c d

arg5 :: (Monoid w) => (r (WS w s) -> a -> b -> c -> d -> e -> WS w s f) -> a -> b -> c -> d -> e -> TestFixture r w s f
arg5 rec a b c d e = do
  fn <- asks rec
  lift $ fn a b c d e

arg6 :: (Monoid w) => (r (WS w s) -> a -> b -> c -> d -> e -> f -> WS w s g) -> a -> b -> c -> d -> e -> f -> TestFixture r w s g
arg6 rec a b c d e f = do
  fn <- asks rec
  lift $ fn a b c d e f

arg7 :: (Monoid w) => (r (WS w s) -> a -> b -> c -> d -> e -> f -> g -> WS w s h) -> a -> b -> c -> d -> e -> f -> g -> TestFixture r w s h
arg7 rec a b c d e f g = do
  fn <- asks rec
  lift $ fn a b c d e f g

unimplemented :: String -> a
unimplemented name = error ("unimplemented fixture method `" ++ name ++ "`")
