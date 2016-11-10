{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.DeepSeq.TemplateHaskell () where

import Control.DeepSeq (NFData)
import Language.Haskell.TH.Syntax

instance NFData AnnTarget
instance NFData Body
instance NFData Callconv
instance NFData Clause
instance NFData Con
instance NFData Dec
instance NFData Exp
instance NFData FamFlavour
instance NFData Fixity
instance NFData FixityDirection
instance NFData Foreign
instance NFData FunDep
instance NFData Guard
instance NFData Inline
instance NFData Lit
instance NFData Match
instance NFData ModName
instance NFData Name
instance NFData NameFlavour
instance NFData NameSpace
instance NFData OccName
instance NFData Pat
instance NFData Phases
instance NFData PkgName
instance NFData Pragma
instance NFData Range
instance NFData Role
instance NFData RuleBndr
instance NFData RuleMatch
instance NFData Safety
instance NFData Stmt
instance NFData Strict
instance NFData TyLit
instance NFData Type
instance NFData TySynEqn
instance NFData TyVarBndr

#if MIN_VERSION_template_haskell(2,11,0)
instance NFData FamilyResultSig
instance NFData InjectivityAnn
instance NFData Overlap
instance NFData SourceStrictness
instance NFData SourceUnpackedness
instance NFData TypeFamilyHead
#endif
