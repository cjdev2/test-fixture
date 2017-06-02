{-# LANGUAGE CPP #-}

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
{-# OPTIONS_GHC -fno-warn-unused-top-binds -fno-warn-redundant-constraints #-}
#else
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
#endif

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Control.Monad.TestFixture.DefaultSignaturesSpec (spec) where

import Test.Hspec

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.TestFixture.TH

class Monad m => MonadFoo m where
  foo :: m ()

  default foo :: (MonadTrans t, MonadFoo m', m ~ t m') => m ()
  foo = lift foo

mkFixture "Fixture" [ts| MonadFoo |]

spec :: Spec
spec = describe "mkFixture" $
  it "can derive classes that use DefaultSignatures" $ example $
    return ()
