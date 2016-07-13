{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Control.Monad.TestFixtureSpec (spec) where

import Test.Hspec

import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

newtype Id a = Id Int
newtype DBError = DBError () deriving (Eq, Show)

data HTTPRequest = GET String
data HTTPResponse = HTTPResponse { responseStatus :: Int }
data HTTPError

class DBRecord a where
  procureRecord :: a

data User = User deriving (Eq, Show)
instance DBRecord User where
  procureRecord = User

class Monad m => DB m where
  fetchRecord :: DBRecord a => Id a -> m (Either DBError a)
  insertRecord :: DBRecord a => a -> m (Either DBError (Id a))

class Monad m => HTTP m where
  sendRequest :: HTTPRequest -> m (Either HTTPError HTTPResponse)

useDBAndHTTP :: (DB m, HTTP m, DBRecord r) => r -> m (Either DBError r)
useDBAndHTTP record = do
  (Right (Id recordId)) <- insertRecord record
  (Right response) <- sendRequest $ GET ("/record/" ++ show recordId)
  fetchRecord $ Id (responseStatus response)

mkFixture "Fixture" [''DB, ''HTTP]

-- At compile time, ensure the fixture type synonyms are generated.
fixturePure :: FixturePure
fixturePure = def :: Fixture (TestFixture Fixture () ())

fixtureLog :: FixtureLog log
fixtureLog = def :: Fixture (TestFixture Fixture log ())

fixtureState :: FixtureState state
fixtureState = def :: Fixture (TestFixture Fixture () s)

fixtureLogState :: FixtureLogState log state
fixtureLogState = def :: Fixture (TestFixture Fixture log state)

spec :: Spec
spec =
  describe "mkFixture" $
    it "generates a fixture type that can be used to stub out methods" $ do
      let fixture = def
            { _fetchRecord = \_ -> return $ Right procureRecord
            , _insertRecord = \_ -> return $ Right (Id 42)
            , _sendRequest = \_ -> return $ Right (HTTPResponse 200)
            }
      let result = unTestFixture (useDBAndHTTP User) fixture
      result `shouldBe` Right User
