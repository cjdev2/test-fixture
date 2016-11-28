{-|
  This module provides a Template Haskell function for automatically generating
  reified typeclass dictionaries for use with "Control.Monad.TestFixture".
  These generated dictionaries can be used with functions like
  'Control.Monad.TestFixture.unTestFixture' and
  'Control.Monad.TestFixture.logTestFixture' to quickly implement monadic
  typeclasses in a way that can be used to “stub out” functionality in unit
  tests.

  The 'mkFixture' function is a Template Haskell code generation tool, which
  generates three things:

    1. A record type that represents a reified typeclass dictionary (or set of
       typeclass dictionaries). The record contains fields that correspond to
       the methods of the provided typeclasses, with ordinary method names
       prefixed with a @_@ character and infix method names prefixed with a @~@
       character.

    2. A 'Default' instance for the generated record type, which automatically
       fills all fields with stub implementations that will throw using
       'unimplemented'.

    3. Typeclass implementations for all of the provided typeclasses using
       'TestFixture' and the generated record type that defer to the
       implementations provided through the reified dictionary.

  In practice, this is used for generate “fixture” types that are used within
  tests. For example, consider some typeclasses that encode side-effectful
  monadic operations:

  > class Monad m => DB m where
  >   fetchRecord :: DBRecord a => Id a -> m (Either DBError a)
  >   insertRecord :: DBRecord a => a -> m (Either DBError (Id a))
  >
  > class Monad m => HTTP m where
  >   sendRequest :: HTTPRequest -> m (Either HTTPError HTTPResponse)

  The typeclasses may have relatively straightforward instances for 'IO'.
  However, one of the main values of them is that alternative instances may be
  provided in unit tests, which is what 'TestFixture' provides. Therefore,
  one might use 'mkFixture' to create some utilities for stubbing these
  typeclasses out:

  > mkFixture "Fixture" [ts| DB, HTTP |]

  This generates code much like the following:

  > data Fixture m =
  >   { _fetchRecord :: DBRecord a => Id a -> m (Either DBError a)
  >   , _insertRecord :: DBRecord a => a -> m (Either DBError (Id a))
  >   , _sendRequest :: HTTPRequest -> m (Either HTTPError HTTPResponse)
  >   }
  >
  > instance Default (Fixture m) where
  >   def = Fixture
  >     { _fetchRecord = unimplemented "_fetchRecord"
  >     , _insertRecord = unimplemented "_insertRecord"
  >     , _sendRequest = unimplemented "_sendRequest"
  >     }
  >
  > type FixturePure = Fixture (TestFixture Fixture () ())
  > type FixtureLog log = Fixture (TestFixture Fixture log ())
  > type FixtureState state = Fixture (TestFixture Fixture () state)
  > type FixtureLogState log state = Fixture (TestFixture Fixture log state)
  >
  > type FixturePureT m = Fixture (TestFixture Fixture () () m)
  > type FixtureLogT log m = Fixture (TestFixture Fixture log () m)
  > type FixtureStateT state m = Fixture (TestFixture Fixture () state m)
  > type FixtureLogStateT log state m = Fixture (TestFixtureT Fixture log state m)
  >
  > instance Monad m => DB (TestFixtureT Fixture w s m) where
  >   fetchRecord r = do
  >     fn <- asks _fetchRecord
  >     fn r
  >   insertRecord r = do
  >     fn <- asks _insertRecord
  >     fn r
  >
  > instance Monad m => HTTP (TestFixtureT Fixture w s m) where
  >   sendRequest r = do
  >     fn <- asks _sendRequest
  >     fn r

  This type can then be used in tandem with "Control.Monad.TestFixture" to
  create stubbed typeclass instances and run computations using them.
-}
module Control.Monad.TestFixture.TH
  ( mkFixture
  , def
  , ts
  ) where

import Control.Monad.TestFixture.TH.Internal (mkFixture)
import Control.Monad.TestFixture.TH.Internal.TypesQuasi (ts)
import Data.Default.Class (def)
