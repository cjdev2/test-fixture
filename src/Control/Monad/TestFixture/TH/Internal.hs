{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.TestFixture.TH.Internal where

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import qualified Control.Monad.Reader as Reader

import Prelude hiding (log)
import Control.Monad (join, replicateM, zipWithM)
import Control.Monad.TestFixture (TestFixture, TestFixtureT, unimplemented)
import Data.Char (isPunctuation, isSymbol)
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
  typeSynonyms <- mkFixtureTypeSynonyms fixtureName
  defaultInstanceDec <- mkDefaultInstance fixtureName fixtureFields

  infos <- traverse reify classNames
  instanceDecs <- traverse (flip mkInstance fixtureName) infos

  return ([fixtureDec, defaultInstanceDec] ++ typeSynonyms ++ instanceDecs)

mkFixtureRecord :: Name -> [Name] -> Q (Dec, [VarStrictType])
mkFixtureRecord fixtureName classNames = do
  types <- traverse conT classNames
  info <- traverse reify classNames
  methods <- traverse classMethods info

  mVar <- newName "m"
  fixtureFields <- join <$> zipWithM (methodsToFields mVar) types methods
  let fixtureCs = [RecC fixtureName fixtureFields]

  let mKind = AppT (AppT ArrowT StarT) StarT
  let fixtureDec = mkDataD [] fixtureName [KindedTV mVar mKind] fixtureCs
  return (fixtureDec, fixtureFields)

mkFixtureTypeSynonyms :: Name -> Q [Dec]
mkFixtureTypeSynonyms fixtureName = do
  mName <- newName "m"
  logName <- newName "log"
  stateName <- newName "state"

  let mVar = VarT mName
  let logVar = VarT logName
  let stateVar = VarT stateName

  let mTVBndr = PlainTV mName
  let logTVBndr = PlainTV logName
  let stateTVBndr = PlainTV stateName

  let fixturePure = mkTypeSynonym "Pure" [] (mkFixtureType unit unit)
  let fixtureLog = mkTypeSynonym "Log" [logTVBndr] (mkFixtureType logVar unit)
  let fixtureState = mkTypeSynonym "State" [stateTVBndr] (mkFixtureType unit stateVar)
  let fixtureLogState = mkTypeSynonym "LogState" [logTVBndr, stateTVBndr] (mkFixtureType logVar stateVar)
  let fixturePureT = mkTypeSynonym "PureT" [mTVBndr] (mkFixtureTransformerType unit unit mVar)
  let fixtureLogT = mkTypeSynonym "LogT" [logTVBndr, mTVBndr] (mkFixtureTransformerType logVar unit mVar)
  let fixtureStateT = mkTypeSynonym "StateT" [stateTVBndr, mTVBndr] (mkFixtureTransformerType unit stateVar mVar)
  let fixtureLogStateT = mkTypeSynonym "LogStateT" [logTVBndr, stateTVBndr, mTVBndr] (mkFixtureTransformerType logVar stateVar mVar)

  return
    [ fixturePure
    , fixtureLog
    , fixtureState
    , fixtureLogState
    , fixturePureT
    , fixtureLogT
    , fixtureStateT
    , fixtureLogStateT
    ]
  where
    unit = TupleT 0
    mkTypeSynonym suffix varBndr ty = TySynD (mkName (nameBase fixtureName ++ suffix)) varBndr ty
    mkFixtureType log state = AppT (ConT fixtureName) (AppT (AppT (AppT (ConT ''TestFixture) (ConT fixtureName)) log) state)
    mkFixtureTransformerType log state m = AppT (ConT fixtureName) (AppT (AppT (AppT (AppT (ConT ''TestFixtureT) (ConT fixtureName)) log) state) m)

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
  mVar <- VarT <$> newName "m"

  let fixtureWithoutVarsT = AppT (ConT ''TestFixtureT) (ConT fixtureName)
  let fixtureT = AppT (AppT (AppT fixtureWithoutVarsT writerVar) stateVar) mVar
  let instanceHead = AppT (ConT className) fixtureT

  funDecls <- traverse mkDictInstanceFunc methods

  return $ mkInstanceD [AppT (ConT ''Monad) mVar] instanceHead funDecls
mkInstance other _ = fail $ "mkInstance: expected a class name, given " ++ show other

{-|
  Given some 'Info' about a class, get its methods as 'SigD' declarations.
-}
classMethods :: MonadFail m => Info -> m [Dec]
classMethods (ClassI (ClassD _ _ _ _ methods) _) = return methods
classMethods other = fail $ "classMethods: expected a class name, given " ++ show other

{-|
  Helper for applying `methodToField` over multiple methods using the same name
  replacement for a particular typeclass.
-}
methodsToFields :: MonadFail m => Name -> Type -> [Dec] -> m [VarStrictType]
methodsToFields name typ = mapM (methodToField name typ)

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
methodToField :: MonadFail m => Name -> Type -> Dec -> m VarStrictType
methodToField mVar classT (SigD name typ) = (fieldName, noStrictness,) <$> newT
  where fieldName = methodNameToFieldName name
        newT = replaceClassConstraint classT mVar typ
methodToField _ _ _ = fail "methodToField: internal error; report a bug with the test-fixture package"

{-|
  Prepends a name with a @_@ or @~@ character (depending on whether or not the
  name refers to an infix operator) to avoid name clashes when generating record
  fields based on typeclass method names.
-}
methodNameToFieldName :: Name -> Name
methodNameToFieldName name = mkName (prefixChar : nameBase name)
  where isInfixChar c = (c `notElem` "_:\"'") && (isPunctuation c || isSymbol c)
        nameIsInfix = isInfixChar . head $ nameBase name
        prefixChar = if nameIsInfix then '~' else '_'

{-|
  Implements the class constraint replacement functionality as described in the
  documentation for 'methodToField'. Given a type that represents the typeclass
  whose constraint must be removed and a name used to replace the constrained
  type variable, it replaces the uses of that type variable everywhere in the
  quantified type and removes the constraint.
-}
replaceClassConstraint :: MonadFail m => Type -> Name -> Type -> m Type
replaceClassConstraint constraint freeVar (ForallT vars preds typ) = do
  let (newPreds, [replacedPred]) = partition ((constraint /=) . unappliedType) preds
  -- check that this is a single-parameter typeclass
  replacedVar <- case typeVarNames replacedPred of
    [singleVar] -> return singleVar
    _           -> fail "generating instances of multi-parameter typeclasses is currently unsupported"
  let newVars = filter ((replacedVar /=) . tyVarBndrName) vars
      replacedT = replaceTypeVarName replacedVar freeVar typ
  return $ ForallT newVars newPreds replacedT
replaceClassConstraint _ _ _ = fail "replaceClassConstraint: internal error; report a bug with the test-fixture package"

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
| The following definitions abstract over differences in base and              |
| template-haskell between GHC versions. This allows the same code to work     |
| without writing CPP everywhere and ending up with a small mess.              |
|------------------------------------------------------------------------------}

#if MIN_VERSION_base(4,9,0)
type MonadFail = Fail.MonadFail
#else
type MonadFail = Monad
#endif

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
