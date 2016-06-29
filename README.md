# test-fixture [![Build Status](https://travis-ci.org/cjdev/test-fixture.svg?branch=master)](https://travis-ci.org/cjdev/test-fixture)

The [test-fixture][test-fixture-hackage] package is a Haskell library that makes it possible to easily write deterministic unit tests for code that encapsulates effects into monadic typeclasses. For example, given some typeclasses used to encapsulate effects:

```haskell
class Monad m => MonadDB m where
  fetchRecord :: DBRecord a => Id a -> m (Either DBError a)
  insertRecord :: DBRecord a => a -> m (Either DBError (Id a))

class Monad m => MonadHTTP m where
  sendRequest :: HTTPRequest -> m (Either HTTPError HTTPResponse)
```

One can write `IO` instances to run the actual code in a real environment:

```haskell
instance MonadDB IO where
  fetchRecord = Postgres.fetchRecord
  insertRecord = Postgres.insertRecord

instance MonadHTTP IO where
  sendRequest = sendRequestIO
```

Then use those typeclasses to implement some sort of side-effectful function:

```haskell
sendAndFetch :: (MonadDB m, MonadHTTP m, DBRecord a)
             => HTTPRequest -> m (Either AppError a) ()
sendAndFetch = ...
```

Testing this function might be difficult because of all the different possible combinations of scenarios that must be considered. Creating lots of different monads and instances for each case can be boilerplate-heavy and tedious. Using test-fixture, the boilerplate is unnecessary:

```haskell
mkFixture "Fixture" [''MonadDB, ''MonadHTTP]

spec = describe "sendAndFetch" $ do
  it "returns a record when the http request and db fetch are successful" $ do
    let (fixture :: Monad m => Fixture m) = def
      { _fetchRecord = \_ -> return $ Right procureRecord
      , _sendRequest = \_ -> return $ Right responseOk
      }
    let result = unTestFixture (sendAndFetch simpleRequest) fixture
    result `shouldBe` Right (User "someone@example.com")

  it "returns an error when the http request is not successful" $ do
    let (fixture :: Monad m => Fixture m) = def
      { _fetchRecord = \_ -> return $ Right procureRecord
      , _sendRequest = \_ -> return $ Left errorNotAuthorized
      }
    let result = unTestFixture (sendAndFetch simpleRequest) fixture
    result `shouldBe` Left (AppHTTPError errorNotAuthorized)
```

For more information and a more complete explanation, [see the documentation on Hackage][test-fixture-hackage].

[test-fixture-hackage]: http://hackage.haskell.org/package/test-fixture
