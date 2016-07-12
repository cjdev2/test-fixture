{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

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
       typeclass dictionaries).

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

  > mkFixture "Fixture" [''DB, ''HTTP]

  This generates code much like the following:

  > data Fixture m =
  >   { _fetchRecord :: DBRecord a => Id a -> m (Either DBError a)
  >   , _insertRecord :: DBRecord a => a -> m (Either DBError (Id a))
  >   , _sendRequest :: HTTPRequest -> m (Either HTTPError HTTPResponse) }
  >
  > instance Default (Fixture m) where
  >   def =
  >     { _fetchRecord = unimplemented "_fetchRecord"
  >     , _insertRecord = unimplemented "_insertRecord"
  >     , _sendRequest = unimplemented "_sendRequest" }
  >
  > instance DB (TestFixture Fixture w s) where
  >   fetchRecord r = do
  >     fn <- asks _fetchRecord
  >     lift $ fn r
  >   insertRecord r = do
  >     fn <- asks _insertRecord
  >     lift $ fn r
  >
  > instance HTTP (TestFixture Fixture w s) where
  >   sendRequest r = do
  >     fn <- asks _sendRequest
  >     lift $ fn r

  This type can then be used in tandem with "Control.Monad.TestFixture" to
  create stubbed typeclass instances and run computations using them.
-}
module Control.Monad.TestFixture.TH
  ( mkFixture
  , def
  ) where

import qualified Control.Monad.Reader as Reader

import Control.Monad (join, replicateM)
import Control.Monad.TestFixture (TestFixture, unimplemented)
import Data.Default (Default(..))
import Data.List (foldl', nub, partition)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

{-|
  A Template Haskell function that generates a fixture record type with a given
  name that reifies the set of typeclass dictionaries provided, as described in
  the module documentation for "Control.Monad.TestFixture.TH". For example, the
  following splice would create a new record type called @Fixture@ with fields
  and instances for typeclasses called @Foo@ and @Bar@:

  > mkFixture "Fixture" [''Foo, ''Bar]
-}
mkFixture :: String -> [Name] -> Q [Dec]
mkFixture fixtureNameStr classNames = do
  let fixtureName = mkName fixtureNameStr

  (fixtureDec, fixtureFields) <- mkFixtureRecord fixtureName classNames
  defaultInstanceDec <- mkDefaultInstance fixtureName fixtureFields

  infos <- traverse reify classNames
  instanceDecs <- traverse (flip mkInstance fixtureName) infos

  return ([fixtureDec, defaultInstanceDec] ++ instanceDecs)

mkFixtureRecord :: Name -> [Name] -> Q (Dec, [VarStrictType])
mkFixtureRecord fixtureName classNames = do
  types <- traverse conT classNames
  info <- traverse reify classNames
  methods <- traverse classMethods info

  mVar <- newName "m"
  let fixtureFields = join $ zipWith (methodsToFields mVar) types methods
  let fixtureCs = [RecC fixtureName fixtureFields]

  let fixtureDec = mkDataD [] fixtureName [PlainTV mVar] fixtureCs
  return (fixtureDec, fixtureFields)

mkDefaultInstance :: Name -> [VarStrictType] -> Q Dec
mkDefaultInstance fixtureName fixtureFields = do
  varName <- newName "m"
  let appliedFixtureT = AppT (ConT fixtureName) (VarT varName)

  let fieldNames = map (\(name, _, _) -> name) fixtureFields
  let fixtureClauses = map unimplementedField fieldNames

  let defImpl = RecConE fixtureName fixtureClauses
  let defDecl = FunD 'def [Clause [] (NormalB defImpl) []]

  return $ mkInstanceD [] (AppT (ConT ''Default) appliedFixtureT) [defDecl]

mkInstance :: Info -> Name -> Q Dec
mkInstance (ClassI (ClassD _ className _ _ methods) _) fixtureName = do
  writerVar <- VarT <$> newName "w"
  stateVar <- VarT <$> newName "s"

  let fixtureWithoutVarsT = AppT (ConT ''TestFixture) (ConT fixtureName)
  let fixtureT = AppT (AppT fixtureWithoutVarsT writerVar) stateVar
  let instanceHead = AppT (ConT className) fixtureT

  funDecls <- traverse mkDictInstanceFunc methods

  return $ mkInstanceD [] instanceHead funDecls
mkInstance other _ = fail $ "mkInstance: expected a class name, given " ++ show other

{-|
  Given some 'Info' about a class, get its methods as 'SigD' declarations.
-}
classMethods :: Info -> Q [Dec]
classMethods (ClassI (ClassD _ _ _ _ methods) _) = return methods
classMethods other = fail $ "classMethods: expected a class name, given " ++ show other

{-|
  Helper for applying `methodToField` over multiple methods using the same name
  replacement for a particular typeclass.
-}
methodsToFields :: Name -> Type -> [Dec] -> [VarStrictType]
methodsToFields name typ = map (methodToField name typ)

{-|
  Converts a typeclass’s method (represented as a 'SigD') to a record field.
  There are two operations involved in this conversion:

    1. Prepend the name with the @_@ character to avoid name clashes. This is
       performed by 'methodNameToFieldName'.

    2. Replace the type variable bound by the typeclass constraint. To explain
       this step, consider the following typeclass:

       > class HasFoo x where
       >   foo :: x -> Foo

       The signature for the @foo@ class is actually as follows:

       > forall x. HasFoo x => x -> Foo

       However, when converted into a record, we want it to look like this:

       > data Record x = Record { fFoo :: x -> Foo }

       Specifically, we want to remove the @forall@ constraint, and we need
       to replace the type variable bound by the typeclass constraint with the
       type variable bound by the record declaration itself.

       To accomplish this, 'methodToField' accepts a 'Name' and a 'Type', where
       the 'Name' is the name of a replacement type variable, and the 'Type'
       is the typeclass whose constraint must be removed.
-}
methodToField :: Name -> Type -> Dec -> VarStrictType
methodToField mVar classT (SigD name typ) = (fieldName, noStrictness, newT)
  where fieldName = methodNameToFieldName name
        newT = replaceClassConstraint classT mVar typ
methodToField _ _ _ = error "internal error; report a bug with the test-fixture package"

{-|
  Prepends a name with the @_@ character to avoid name clashes when generating
  record fields based on typeclass method names.
-}
methodNameToFieldName :: Name -> Name
methodNameToFieldName name = mkName ('_' : nameBase name)

{-|
  Implements the class constraint replacement functionality as described in the
  documentation for 'methodToField'. Given a type that represents the typeclass
  whose constraint must be removed and a name used to replace the constrained
  type variable, it replaces the uses of that type variable everywhere in the
  quantified type and removes the constraint.
-}
replaceClassConstraint :: Type -> Name -> Type -> Type
replaceClassConstraint constraint freeVar (ForallT vars preds typ) = ForallT newVars newPreds replacedT
  where (newPreds, [replacedPred]) = partition ((constraint /=) . unappliedType) preds
        [replacedVar] = typeVarNames replacedPred
        newVars = filter ((replacedVar /=) . tyVarBndrName) vars
        replacedT = replaceTypeVarName replacedVar freeVar typ
replaceClassConstraint _ _ _ = error "internal error; report a bug with the test-fixture package"

{-|
  Performs an alpha-renaming within a particular type. Of course, a pure alpha-
  renaming would be pretty useless, but this function can be useful because it
  it unhygienic in the sense that type variables can be replaced with others
  with separate bindings.

  This is used by 'replaceClassConstraint' to swap out the constrained and
  quantified type variable with the type variable bound within the record
  declaration.
-}
replaceTypeVarName :: Name -> Name -> Type -> Type
replaceTypeVarName initial replacement = doReplace
  where doReplace (ForallT a b t) = ForallT a b (doReplace t)
        doReplace (AppT a b) = AppT (doReplace a) (doReplace b)
        doReplace (SigT t k) = SigT (doReplace t) k
        doReplace (VarT n)
          | n == initial = VarT replacement
          | otherwise    = VarT n
        doReplace other = other

{-|
  Given a record field name, produces a 'FieldExp' that assigns that field to
  a function defined in terms of 'unimplemented', which will raise an error
  upon an attempt to invoke it that will contain a message that explains the
  method has not been implemented by a user.
-}
unimplementedField :: Name -> FieldExp
unimplementedField fieldName = (fieldName, unimplementedE)
  where unimplementedE = AppE (VarE 'unimplemented) (LitE (StringL $ nameBase fieldName))

{-|
  Generates an implementation of a method within a 'TestFixture' typeclass
  instance for a generated fixture record. The implementation handles three
  things:

    1. It detects the arity of the method to implement and automatically creates
       a function declaration that accepts that many arguments.

    2. It retrieves the actual implementation out of the reader-provided
       typeclass dictionary using 'asks'.

    3. It applies the reader-provided function to all of the arguments generated
       by the arity-detection pass from step 1.

   This function expects a signature declaration that describes the typeclass
   method to generate an implementation for, and it returns the function
   definition as a declaration.
-}
mkDictInstanceFunc :: Dec -> Q Dec
mkDictInstanceFunc (SigD name typ) = do
  let arity = functionTypeArity typ

  argNames <- replicateM arity (newName "x")
  let pats = map VarP argNames

  let askFunc = VarE (methodNameToFieldName name)
  let vars = map VarE argNames

  implE <- [e|do
    fn <- Reader.asks $(return askFunc)
    $(return $ applyE (VarE 'fn) vars)|]

  let funClause = Clause pats (NormalB implE) []
  return $ FunD name [funClause]
mkDictInstanceFunc other = fail $ "mkDictInstanceFunc: expected method signature, given " ++ show other

{-|
  Given a potentially applied type, like @T a b@, returns the base, unapplied
  type name, like @T@.
-}
unappliedType :: Type -> Type
unappliedType t@ConT{} = t
unappliedType (AppT t _) = unappliedType t
unappliedType other = error $ "expected plain applied type, given " ++ show other

{-|
  Given a type, returns a list of all of the unique type variables contained
  within it.
-}
typeVarNames :: Type -> [Name]
typeVarNames (VarT n) = [n]
typeVarNames (AppT a b) = nub (typeVarNames a ++ typeVarNames b)
typeVarNames _ = []

{-|
  Given any arbitrary 'TyVarBndr', gets its 'Name'.
-}
tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV name) = name
tyVarBndrName (KindedTV name _) = name

{-|
  Given any arbitrary 'Type', gets its function arity as a 'Int'. Non-function
  types have arity @0@.

  >>> functionTypeArity [t|()|]
  0
  >>> functionTypeArity [t|() -> ()|]
  1
  >>> functionTypeArity [t|() -> () -> ()|]
  2
-}
functionTypeArity :: Type -> Int
functionTypeArity (AppT (AppT ArrowT _) b) = 1 + functionTypeArity b
functionTypeArity (ForallT _ _ typ) = functionTypeArity typ
functionTypeArity _ = 0

{-|
  Given an 'Exp' that represents a function value and a list of 'Exp's that
  represent function arguments, produces a new 'Exp' that applies the function
  to the provided arguments.
-}
applyE :: Exp -> [Exp] -> Exp
applyE = foldl' AppE

{------------------------------------------------------------------------------|
| The following functions abstract over differences in template-haskell        |
| between GHC versions. This allows the same code to work without writing CPP  |
| everywhere and ending up with a small mess.                                  |
|------------------------------------------------------------------------------}

mkInstanceD :: Cxt -> Type -> [Dec] -> Dec
#if MIN_VERSION_template_haskell(2,11,0)
mkInstanceD = InstanceD Nothing
#else
mkInstanceD = InstanceD
#endif

mkDataD :: Cxt -> Name -> [TyVarBndr] -> [Con] -> Dec
#if MIN_VERSION_template_haskell(2,11,0)
mkDataD a b c d = DataD a b c Nothing d []
#else
mkDataD a b c d = DataD a b c d []
#endif

#if MIN_VERSION_template_haskell(2,11,0)
noStrictness :: Bang
noStrictness = Bang NoSourceUnpackedness NoSourceStrictness
#else
noStrictness :: Strict
noStrictness = NotStrict
#endif
