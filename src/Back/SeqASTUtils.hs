module Back.SeqASTUtils where

import Back.SeqAST

extractVarName :: VarDescr a -> String
extractVarName (SimpleName n, _) = n
extractVarName (_, _) = error "SeqASTUtils.extractName"

extractVarType :: VarDescr a -> PiccType a
extractVarType = snd

extractValType :: Value a -> PiccType a
extractValType = snd
