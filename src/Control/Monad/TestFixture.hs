{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

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

  To solve this, the provided 'TestFixture' monad combines a /reader/, /writer/,
  and /state/ monad into a single system. This allows “logging” results from a
  fixture by using 'tell' within the fixture definition and 'logTestFixture',
  and it also permits having fixture invocations depend on previous uses of the
  fixture by using 'get' and 'put' from 'MonadState'.

  Continuing from the above example but using 'TestFixture' instead, we eschew
  the simpler @FixtureM@ type and create instances over 'TestFixture' instead:

  > instance Monoid log => LookupUser (TestFixture Fixture log state) where
  >   lookupUser userId = do
  >     fn <- asks _lookupUser
  >     lift $ fn userId

  Now we can write our tests using the 'unTestFixture' function, along with the
  similar 'logTestFixture' functions and friends:

  > spec = describe "lookupUserIsAdmin" $ do
  >   it "returns True when the UserId corresponds to an admin user" $ do
  >     let fixture = Fixture { _lookupUser = return $ Just User { isAdmin = True } }
  >     unTestFixture (lookupUserIsAdmin (UserId 42)) fixture `shouldBe` True
  >
  >   it "returns False when the UserId corresponds to a non-admin user" $ do
  >     let fixture = Fixture { _lookupUser = return $ Just User { isAdmin = False } }
  >     unTestFixture (lookupUserIsAdmin (UserId 42)) fixture `shouldBe` False
  >
  >   it "returns False when the UserId does not have a corresponding User" $ do
  >     let fixture = Fixture { _lookupUser = return Nothing }
  >     unTestFixture (lookupUserIsAdmin (UserId 42)) fixture `shouldBe` False

  As a final note, writing out all of these fixture record definitions and
  instance declarations can be extremely tedious with large numbers of
  typeclasses and tests. To mitigate this, the "Control.Monad.TestFixture.TH"
  module provides a 'Control.Monad.TestFixture.TH.mkFixture' function, which
  uses Template Haskell to generate the necessary code instead.
-}
module Control.Monad.TestFixture (
  -- * The TestFixture monad
    TestFixture
  , unTestFixture
  , logTestFixture
  , evalTestFixture
  , execTestFixture
  , runTestFixture
  -- * The TestFixtureT monad transformer
  , TestFixtureT
  , unTestFixtureT
  , logTestFixtureT
  , evalTestFixtureT
  , execTestFixtureT
  , runTestFixtureT
  -- * Helper functions
  , module Control.Monad.Writer.Class
  , module Control.Monad.State.Class
  , arg0
  , arg1
  , arg2
  , arg3
  , arg4
  , arg5
  , arg6
  , arg7
  , unimplemented
  , log
  ) where

import Prelude hiding (log)

import qualified Control.Monad.Writer.Class
import qualified Control.Monad.State.Class

import Control.Monad.RWS
import Data.Functor.Identity

-- | The 'TestFixture' monad. A combination of a /reader/, /writer/, and /state/
--   monad, where the reader portion contains a reified typeclass dictionary
--   used as a fixture. For more information, see the module documentation for
--   "Control.Monad.TestFixture".
type TestFixture fixture log state = TestFixtureT fixture log state Identity

-- | 'TestFixture' as a monad transformer instead of as a monad.
newtype TestFixtureT fixture log state m a = TestFixtureT { getRWST :: RWST (fixture (TestFixtureT fixture log state m)) [log] state m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (fixture (TestFixtureT fixture log state m))
    , MonadWriter [log]
    , MonadState state
    )

instance MonadTrans (TestFixtureT fixture log state) where
  lift = TestFixtureT . lift

-- | The transformer equivalent of 'unTestFixture'.
unTestFixtureT :: Monad m => TestFixtureT fixture log state m a -> fixture (TestFixtureT fixture log state m) -> m a
unTestFixtureT stack env = fmap fst (evalTestFixtureT stack env)

-- | The transformer equivalent of 'logTestFixture'.
logTestFixtureT :: Monad m => TestFixtureT fixture log state m a -> fixture (TestFixtureT fixture log state m) -> m [log]
logTestFixtureT stack env = fmap snd (evalTestFixtureT stack env)

-- | The transformer equivalent of 'evalTestFixture'.
evalTestFixtureT :: Monad m => TestFixtureT fixture log state m a -> fixture (TestFixtureT fixture log state m) -> m (a, [log])
evalTestFixtureT stack env = evalRWST (getRWST stack) env ()

-- | The transformer equivalent of 'execTestFixture'.
execTestFixtureT :: Monad m => TestFixtureT fixture log state m a -> fixture (TestFixtureT fixture log state m) -> state -> m (state, [log])
execTestFixtureT stack env st = execRWST (getRWST stack) env st

-- | The transformer equivalent of 'runTestFixture'.
runTestFixtureT :: Monad m => TestFixtureT fixture log state m a -> fixture (TestFixtureT fixture log state m) -> state -> m (a, state, [log])
runTestFixtureT stack env st = runRWST (getRWST stack) env st

{-|
  The simplest way to run a test given a fixture, 'unTestFixture' simply runs a
  monadic computation with a particular fixture and returns the computation’s
  result. Useful for testing impure functions that return useful values.
-}
unTestFixture
  :: TestFixture fixture log state a         -- ^ the monadic computation to run
  -> fixture (TestFixture fixture log state) -- ^ the fixture dictionary to use
  -> a                                   -- ^ the computation’s result
unTestFixture stack env = runIdentity (unTestFixtureT stack env)

{-|
  Like 'unTestFixture', but instead of returning the result of the computation,
  'logTestFixture' returns the value written from the writer monad. Useful for
  testing impure functions called exclusively for side-effects that do not
  depend on complex prior state.
-}
logTestFixture :: TestFixture fixture log state a -> fixture (TestFixture fixture log state) -> [log]
logTestFixture stack env = runIdentity (logTestFixtureT stack env)

{-|
  Combines 'unTestFixture' and 'logTestFixture' to return /both/ the
  computation’s result and the written value as a tuple.
-}
evalTestFixture :: TestFixture fixture log state a -> fixture (TestFixture fixture log state) -> (a, [log])
evalTestFixture stack env = runIdentity (evalTestFixtureT stack env)

{-|
  Like 'logTestFixture' but accepts an initial state and returns the final
  monadic state tupled with the value written from the writer monad. Useful for
  testing stateful side-effectful computations.
-}
execTestFixture :: TestFixture fixture log state a -> fixture (TestFixture fixture log state) -> state -> (state, [log])
execTestFixture stack env st = runIdentity (execTestFixtureT stack env st)

{-|
  Runs a test fixture given an initial state and returns all three pieces of
  resulting information: the computation’s result, the final monadic state, and
  the value written from the writer.
-}
runTestFixture :: TestFixture fixture log state a -> fixture (TestFixture fixture log state) -> state -> (a, state, [log])
runTestFixture stack env st = runIdentity (runTestFixtureT stack env st)

{-|
  A helper function for implementing typeclass instances over 'TestFixture' that
  pull a value out of a monadic dictionary. For example, given the following
  instance:

  > instance Monoid log => MonadSomething (TestFixture Fixture log state) where
  >   getSomething = do
  >     something <- asks _getSomething
  >     lift something

  Using 'arg0', it can be rewritten like this:

  > instance Monoid log => MonadSomething (TestFixture Fixture log state) where
  >   getSomething = arg0 _getSomething

  For functions of various arities instead of plain values, use 'arg1' through
  'arg7', instead.
-}
arg0 :: (fixture (TestFixture fixture log state) -> TestFixture fixture log state a) -> TestFixture fixture log state a
arg0 rec = join $ asks rec

{-|
  Like 'arg0', but for lifting record accessors containing functions of arity
  one. For example, given the following instance:

  > instance Monoid log => MonadSomething (TestFixture Fixture log state) where
  >   doSomething x = do
  >     fn <- asks _doSomething
  >     lift $ fn x

  Using 'arg1', it can be rewritten like this:

  > instance Monoid log => MonadSomething (TestFixture Fixture log state) where
  >   doSomething = arg1 _doSomething

  For functions of higher arities, use 'arg2' through 'arg7'.
-}
arg1 :: Monad m => (fixture (TestFixtureT fixture log state m) -> a -> TestFixtureT fixture log state m b) -> a -> TestFixtureT fixture log state m b
arg1 rec a = do
  fn <- asks rec
  fn a

-- | Like 'arg1', but for functions of arity 2.
arg2 :: Monad m => (fixture (TestFixtureT fixture log state m) -> a -> b -> TestFixtureT fixture log state m c) -> a -> b -> TestFixtureT fixture log state m c
arg2 rec a b = do
  fn <- asks rec
  fn a b

-- | Like 'arg1', but for functions of arity 3.
arg3 :: Monad m => (fixture (TestFixtureT fixture log state m) -> a -> b -> c -> TestFixtureT fixture log state m d) -> a -> b -> c -> TestFixtureT fixture log state m d
arg3 rec a b c = do
  fn <- asks rec
  fn a b c

-- | Like 'arg1', but for functions of arity 4.
arg4 :: Monad m => (fixture (TestFixtureT fixture log state m) -> a -> b -> c -> d -> TestFixtureT fixture log state m e) -> a -> b -> c -> d -> TestFixtureT fixture log state m e
arg4 rec a b c d = do
  fn <- asks rec
  fn a b c d

-- | Like 'arg1', but for functions of arity 5.
arg5 :: Monad m => (fixture (TestFixtureT fixture log state m) -> a -> b -> c -> d -> e -> TestFixtureT fixture log state m f) -> a -> b -> c -> d -> e -> TestFixtureT fixture log state m f
arg5 rec a b c d e = do
  fn <- asks rec
  fn a b c d e

-- | Like 'arg1', but for functions of arity 6.
arg6 :: Monad m => (fixture (TestFixtureT fixture log state m) -> a -> b -> c -> d -> e -> f -> TestFixtureT fixture log state m g) -> a -> b -> c -> d -> e -> f -> TestFixtureT fixture log state m g
arg6 rec a b c d e f = do
  fn <- asks rec
  fn a b c d e f

-- | Like 'arg1', but for functions of arity 7.
arg7 :: Monad m => (fixture (TestFixtureT fixture log state m) -> a -> b -> c -> d -> e -> f -> g -> TestFixtureT fixture log state m h) -> a -> b -> c -> d -> e -> f -> g -> TestFixtureT fixture log state m h
arg7 rec a b c d e f g = do
  fn <- asks rec
  fn a b c d e f g

{-|
  An extremely simple helper function for creating “base” fixture dictionaries
  with implementations that will simply throw as soon as they are called using
  a helpful error message. The provided argument should be the name of a method
  being implemented.

  >>> unimplemented "_getSomething"
  *** Exception: unimplemented fixture method `_getSomething`
-}
unimplemented :: String -> a
unimplemented name = error ("unimplemented fixture method `" ++ name ++ "`")

{-|
  Logs a single value using 'MonadWriter' when the writer state is a list.
  Equivalent to @'tell' . 'pure'@. Useful with 'TestFixture' implementations to
  record values when using functions like 'logTestFixture'.
-}
log :: MonadWriter [log] m => log -> m ()
log = tell . pure
