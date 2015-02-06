{-|
Module         : Utils.PilPrinter
Description    : Piccolo AST pretty printing module
Stability      : experimental

...
--}

module Utils.PilPPrinter where

import Front.AST

import Text.PrettyPrint


class PrettyPrintable a where
  prettyPrint :: a -> String


ppTypeExpr :: TypeExpr -> Doc
ppTypeExpr TUnknown = text "*unknown*"
ppTypeExpr TAtom { typAtom = atom } = ppTypeAtom atom
ppTypeExpr TChannel { typExpr = expr } =
  text "chan" <> char '<' <> ppTypeExpr expr <> char '>'
ppTypeExpr TTuple { typExprs = exprs } =
  char '(' <> hsep (punctuate (text " * ") $ map ppTypeExpr exprs) <> char ')'

instance PrettyPrintable TypeExpr where
  prettyPrint = render . ppTypeExpr


ppTypeAtom :: TypeAtom -> Doc
ppTypeAtom TBool   = text "bool"
ppTypeAtom TInt    = text "int"
ppTypeAtom TString = text "string"

instance PrettyPrintable TypeAtom where
  prettyPrint = render . ppTypeAtom


ppValue :: Value -> Doc
ppValue VTrue {} = text "true"
ppValue VFalse {} = text "false"
ppValue VInt { valInt = i } = int i
ppValue VString { valStr = s } = char '"' <> text s <> char '"'
ppValue VTuple { valVals = vs } =
  char '(' <> hsep (punctuate (text ", ") $ map ppValue vs) <> char ')'
ppValue VVar { valVar = x } = text x
ppValue VPrim { valModule = m, valName = n, valArgs = args } =
  char '#' <> text m <> char ':' <> text n <> char '(' <> argsDoc <> char ')'
  where argsDoc = hsep $ punctuate (text ", ") $ map ppValue args

instance PrettyPrintable Value where
  prettyPrint = render . ppValue


ppProcess :: Process -> Doc
ppProcess PEnd {} = text "end"
ppProcess PChoice { procBranches = bs } =
  sep $ punctuate (text " +\n") $ map ppBranch bs
ppProcess PCall { procModule = m, procName = n, procArgs = args } =
  char '#' <> text m <> char ':' <> text n <> char '(' <> argsDoc <> char ')'
  where argsDoc = hsep $ punctuate (text ", ") $ map ppValue args

instance PrettyPrintable Process where
  prettyPrint = render . ppProcess


ppBranch :: Branch -> Doc
ppBranch Branch { bGuard = g, bAction = act, bCont = p } =
  char '[' <> ppValue g <> char ']' <+> ppAction act <> char ',' <+> ppProcess p
  

instance PrettyPrintable Branch where
  prettyPrint = render . ppBranch


ppAction :: Action -> Doc
ppAction ATau {} = text "tau"
ppAction AOutput { actChan = c, actData = v } =
  text c <> char '!' <> ppValue v
ppAction AInput { actChan = c, actBind = x } =
  text c <> char '?' <> char '(' <> text x <> char ')'
ppAction ANew { actBind = x, actTyp = typ } =
  text "new" <> char '(' <> text x <> char ':' <> ppTypeExpr typ <> char ')'
ppAction ALet { actBind = x, actTyp = typ, actVal = v } =
  text "let" <> char '(' <> text x <> char ':' <> ppTypeExpr typ <+> char '=' <+> ppValue v <> char ')'
ppAction ASpawn { actModule = m, actName = f, actArgs = args } =
  text "spawn" <> char '{' <> callDoc <> char '}'
  where callDoc = char '#' <> text m <> char ':' <> text f <> char '(' <> argsDoc <> char ')'
        argsDoc = hsep $ punctuate (text ", ") $ map ppValue args
ppAction APrim { actModule = m, actName = f, actArgs = args } =
  char '#' <> text m <> char ':' <> text f <> char '(' <> argsDoc <> char ')'
  where argsDoc = hsep $ punctuate (text ", ") $ map ppValue args
  

instance PrettyPrintable Action where
  prettyPrint = render . ppAction


ppDefinition :: Definition -> Doc
ppDefinition Definition { defName = name, defParams = params, defBody = body } =
  text "def" <+> text name <> char '(' <> paramsDoc <> char ')' <+> char '=' $$ nest 2 bodyDoc
  where paramDoc (n,t,_) = text n <> char ':' <> ppTypeExpr t
        paramsDoc = sep $ punctuate (text ", ") $ map paramDoc params
        bodyDoc   = ppProcess body

instance PrettyPrintable Definition where
  prettyPrint = render . ppDefinition


ppModuleDef :: ModuleDef -> Doc
ppModuleDef ModuleDef { moduleName = name, moduleDefs = defs } =
  sep $ punctuate (text "\n") $ (text "module" <+> text name) : map ppDefinition defs

instance PrettyPrintable ModuleDef where
  prettyPrint = render . ppModuleDef

