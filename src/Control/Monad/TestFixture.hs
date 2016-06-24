{-|
  = Introduction and motivation

  This package provides a set of helper functions and types that are designed to
  assist with writing tests for functions that encode side-effects into monads
  using effect-specific typeclasses. Consider a function that performs some sort
  of side effect, such as a function that looks up a user from a database:

  > lookupUser :: UserId -> IO (Maybe User)

  Now consider a function that uses the @lookupUser@ function:

  > lookupUserIsAdmin :: UserId -> IO Bool
  > lookupUserIsAdmin userId = do
  >   maybeUser <- lookupUser userId
  >   return $ maybe False isAdmin maybeUser

  This function works fine, but it’s very difficult to test, even though it is
  extremely simple. Since @lookupUser@ just runs in 'IO', it isn’t easy to test
  @lookupUserIsAdmin@ in isolation. To fix this, it’s possible to create a
  layer of indirection between @lookupUserIsAdmin@ and @lookupUser@ by making
  @lookupUser@ a method of a typeclass instead of a free function:

  > class Monad m => LookupUser m where
  >   lookupUser :: UserId -> m (Maybe User)

  Implementing the original, 'IO'-bound version of @lookupUser@ is easy; we just
  create a @LookupUser@ instance for 'IO':

  > instance LookupUser IO where
  >   lookupUser = lookupUserIO

  However, we can also create other monads that implement the @LookupUser@
  typeclass. For example, we could create a very simple newtype wrapper around
  'Data.Functor.Identity.Identity' with an implementation that /always/ returns
  a user successfully:

  > newtype SuccessMonad a = SuccessMonad (Identity a)
  >   deriving (Functor, Applicative, Monad)
  >
  > runSuccess :: SuccessMonad a -> a
  > runSuccess (SuccessMonad (Identity x)) = x
  >
  > instance LookupUser SuccessMonad where
  >   lookupUser _ = return $ Just User { isAdmin = True }

  Now we can test @lookupUserIsAdmin@ completely deterministically without ever
  needing to touch a real database (using hspec syntax as an example):

  > lookupUserIsAdmin :: LookupUser m => UserId -> m Bool
  > lookupUserIsAdmin userId = do
  >   maybeUser <- lookupUser userId
  >   return $ maybe False isAdmin maybeUser
  >
  > spec = describe "lookupUserIsAdmin" $ do
  >   it "returns True when the UserId corresponds to an admin user" $
  >     runSuccess (lookupUserIsAdmin (UserId 42)) `shouldBe` True

  Similarly, we can also test the failure case by creating a monad that will
  always return 'Nothing':

  > newtype FailureMonad a = FailureMonad (Identity a)
  >   deriving (Functor, Applicative, Monad)
  >
  > runFailure :: FailureMonad a -> a
  > runFailure (FailureMonad (Identity x)) = x
  >
  > instance LookupUser FailureMonad where
  >   lookupUser _ = return Nothing
  >
  >   it "returns False when the UserId does not have a corresponding User" $
  >     runFailure (lookupUserIsAdmin (UserId 42)) `shouldBe` False

  This is great, but it comes at a pretty significant cost: lots and lots of
  boilerplate. It could get even worse when you have a typeclass with many
  methods, or even multiple typeclasses at a time! Clearly, there needs to be
  some way to abstract this pattern a little bit to make it easier to use.

  = Creating a customizable monad

  To permit creating easily customizable implementations of monadic interfaces,
  we can /reify/ a typeclass at the value level by creating a record type with
  a field that corresponds to each method:

  > data Fixture m = Fixture { _lookupUser :: UserId -> m (Maybe User) }

  We have to prefix each method name with an underscore to avoid name clashes,
  but now we have the ability to create a first-class value that represents
  a particular implementation of the @LookupUser@ typeclass. The next step
  is turning one of these values into something that can actually be supplied as
  a monad implementation. One way to do this is to use a reader monad to thread
  a particular 'Fixture' value around. We can create a newtype that will do that
  for us:

  > newtype FixtureM a = FixtureM (Fixture Identity -> a)
  >   deriving (Functor, Applicative, Monad)
  >
  > runFixture :: Fixture Identity -> FixtureM a -> a
  > runFixture fixture (FixtureM func) = func fixture

  By making this new `FixtureM` type an instance of `LookupUser`, we can use
  the @runFixture@ function that we defined to run a particular computation with
  any arbitrary fixture at runtime:

  > instance LookupUser FixtureM where
  >   lookupUser userId = FixtureM $ \fixture ->
  >     runIdentity $ _lookupUser fixture userId

  Now we can write all our tests using one-off fixture implementations without
  creating entirely new types:

  > spec = describe "lookupUserIsAdmin" $ do
  >   it "returns True when the UserId corresponds to an admin user" $ do
  >     let fixture = Fixture { _lookupUser = return $ Just User { isAdmin = True } }
  >     runFixture fixture (lookupUserIsAdmin (UserId 42)) `shouldBe` True
  >
  >   it "returns False when the UserId corresponds to a non-admin user" $ do
  >     let fixture = Fixture { _lookupUser = return $ Just User { isAdmin = False } }
  >     runFixture fixture (lookupUserIsAdmin (UserId 42)) `shouldBe` False
  >
  >   it "returns False when the UserId does not have a corresponding User" $ do
  >     let fixture = Fixture { _lookupUser = return Nothing }
  >     runFixture fixture (lookupUserIsAdmin (UserId 42)) `shouldBe` False

  = Moving beyond a reader

  The above example is relatively contrived, but it may be possible to see how
  this technique could be applied to a larger set of monadic typeclasses by
  creating more instances on a fixture with more methods.

  However, it is sometimes useful to do /even more/ with a fixture, such as
  verifying that a given function was called with a particular argument. For
  example, consider a function with the following signature:

  > insertUser :: User -> m ()

  In this case, testing the /result/ is likely not particulary interesting, but
  testing that the function itself is called with the right argument might be
  helpful. Even more subtly, a function might be called multiple times, and it
  might need to return different values each time! This requires some degree of
  state tracking that a reader monad simply cannot provide.

  To solve this, the provided 'TestFixture' monad is a wrapper aroud the 'RWS'
  monad, which combines a /reader/, /writer/, and /state/ monad into a single
  system. This allows “logging” results from a fixture by using 'tell' within
  the fixture definition and 'logTestFixture', and it also permits having
  fixture invocations depend on previous uses of the fixture by using 'get' and
  'put' from 'MonadState'.

  Continuing from the above example but using 'TestFixture' instead, we eschew
  the simpler @FixtureM@ type and create instances over 'TestFixture' instead:

  > instance Monoid w => LookupUser (TestFixture Fixture w s) where
  >   lookupUser userId = do
  >     fn <- asks _lookupUser
  >     lift $ fn userId

  Now we can write our tests using the 'unTestFixture' function, along with the
  similar 'logTestFixture' functions and friends:

  > spec = describe "lookupUserIsAdmin" $ do
  >   it "returns True when the UserId corresponds to an admin user" $ do
  >     let fixture = Fixture { _lookupUser = return $ Just User { isAdmin = True } }
  >     unTestFixture (lookupUserIsAdmin (UserId 42)) fixture () `shouldBe` True
  >
  >   it "returns False when the UserId corresponds to a non-admin user" $ do
  >     let fixture = Fixture { _lookupUser = return $ Just User { isAdmin = False } }
  >     unTestFixture (lookupUserIsAdmin (UserId 42)) fixture () `shouldBe` False
  >
  >   it "returns False when the UserId does not have a corresponding User" $ do
  >     let fixture = Fixture { _lookupUser = return Nothing }
  >     unTestFixture (lookupUserIsAdmin (UserId 42)) fixture () `shouldBe` False

  As a final note, writing out all of these fixture record definitions and
  instance declarations can be extremely tedious with large numbers of
  typeclasses and tests. To mitigate this, the "Control.Monad.TestFixture.TH"
  module provides a 'Control.Monad.TestFixture.TH.mkFixture' function, which
  uses Template Haskell to generate the necessary code instead.
-}
module Control.Monad.TestFixture (
  -- * The TestFixture monad
    TestFixture
  , WS
  , unTestFixture
  , logTestFixture
  , evalTestFixture
  , execTestFixture
  , runTestFixture
  -- * The TestFixtureT monad transformer
  , TestFixtureT
  , WST
  , unTestFixtureT
  , logTestFixtureT
  , evalTestFixtureT
  , execTestFixtureT
  , runTestFixtureT
  -- * Helper functions
  , module Control.Monad.RWS.Class
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
