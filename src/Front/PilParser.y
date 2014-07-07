{
{-|
Module         : Front.PilParser
Description    : Piccolo parser
stability      : experimental

This module uses Happy parsing tool to parse piccolo file.
-}
module Front.PilParser (parseModule) where

import Front.AST
import Front.ASTUtils
import Front.PilLexer
import PiccError

import Control.Arrow
}

%name parse
%tokentype { Token }
%monad { Alex } { >>= } { return }
%lexer { lexCont } { TokenEOF {} }
%error { parseError }

%token
  an_int         { TokenVInt {}       }
  a_string       { TokenVString {}    }
  'true'         { TokenVBool { contentBool = True  } }
  'false'        { TokenVBool { contentBool = False } }
  'module'       { TokenModule {}     }
  'def'          { TokenDef {}        }
  'end'          { TokenEnd {}        }
  'tau'          { TokenTau {}        }
  'new'          { TokenNew {}        }
  'spawn'        { TokenSpawn {}      }
  'let'          { TokenLet {}        }
  'chan'         { TokenChan {}       }
  'bool'         { TokenBool {}       }
  'int'          { TokenInt {}        }
  'string'       { TokenString {}     }
  '<'            { TokenInf {}        }
  '>'            { TokenSup {}        }
  '('            { TokenLParen {}     }
  ')'            { TokenRParen {}     }
  '['            { TokenLBrack {}     }
  ']'            { TokenRBrack {}     }
  '{'            { TokenLCurly {}     }
  '}'            { TokenRCurly {}     }
  ','            { TokenComma {}      }
  '/'            { TokenSlash {}      }
  '='            { TokenEq {}         }
  ':'            { TokenColon {}      }
  '+'            { TokenPlus {}       }
  '!'            { TokenOut {}        }
  '?'            { TokenIn {}         }
  '#'            { TokenSharp {}      }
  '*'            { TokenStar {}       }
  an_ident       { TokenIdent {}      }

%left '+'
%right ','
%left '?' '!'

%%

ModuleDef :: { Modul }
ModuleDef : 'module' ModuleID Definitions { Modul $2 $3 $ mkLoc' (defLoc $ head $3) (defLoc $ last $3) }

ModuleID :: { String }
ModuleID : an_ident              { contentString $1 }
         | an_ident '/' ModuleID { contentString $1 ++ "/" ++ $3 }

Definitions :: { [Definition] }
Definitions : Definition             { [$1] }
            | Definition Definitions { $1:$2 }

Definition :: { Definition }
Definition : 'def' an_ident ParamList '=' Process { Definition (contentString $2) $3 $5 (-1) $ mkLoc' (tokenLoc $1) (procLoc $5) }

ParamList :: { [(String,TypeExpr,Location)] }
ParamList : '(' ')'        { [] }
          | '(' Params ')' { $2 }

Params :: { [(String,TypeExpr,Location)] }
Params : Param            { [$1] }
       | Param ',' Params { $1:$3 }

Param :: { (String,TypeExpr,Location) }
Param : an_ident ':' TypeDef { (contentString $1, $3, mkLoc' (tokenLoc $1) (typLoc $3)) }
      | an_ident             { (contentString $1, TUnknown noLoc, tokenLoc $1) }

Process :: { Process }
Process : 'end'         { PEnd $ tokenLoc $1 }
        | Call          { $1 }
        | ChoiceProcess { PChoice $1 $ mkLoc' (brLoc $ head $1) (brLoc $ last $1) }
        | PrefixProcess { let (act,cont) = $1 in PPrefix act cont $ mkLoc' (actLoc act) (procLoc cont) }

Call :: { Process }
Call : '#' ModuleID ':' an_ident '(' ')'        { PCall $2 (contentString $4) [] $ mkLoc $1 $6 }
     | '#' ModuleID ':' an_ident '(' Values ')' { PCall $2 (contentString $4) $6 $ mkLoc $1 $7 }
     | an_ident '(' ')'                         { PCall "" (contentString $1) [] $ mkLoc $1 $3 }
     | an_ident '(' Values ')'                  { PCall "" (contentString $1) $3 $ mkLoc $1 $4 }

ChoiceProcess :: { [Branch] }
ChoiceProcess : Branch                   { [$1] }
              | Branch '+' ChoiceProcess { $1:$3 }

Branch :: { Branch }
Branch : '[' Value ']' 'tau' ',' Process                         { BTau $2 $6 $ mkLoc' (tokenLoc $1) (procLoc $6) }
       | '[' Value ']' an_ident '!' Value ',' Process            { BOutput $2 (contentString $4) $6 (-1) $8 $ mkLoc' (tokenLoc $1) (procLoc $8) }
       | '[' Value ']' an_ident '?' '(' an_ident ')' ',' Process { BInput $2 (contentString $4) (contentString $7) (-1) (-1) $10 $ mkLoc' (tokenLoc $1) (procLoc $10) }

PrefixProcess :: { (Action, Process) }
PrefixProcess : Action ',' Process { ($1, $3) }

Action :: { Action }
Action : an_ident '!' Value                           { AOutput (contentString $1) $3 (-1) $ mkLoc' (tokenLoc $1) (valLoc $3) }
       | an_ident '?' '(' an_ident ')'                { AInput (contentString $1) (contentString $4) (-1) (-1) $ mkLoc $1 $5 }
       | 'new' '(' an_ident ':' TypeDef ')'           { ANew (contentString $3) (-1) $5 $ mkLoc $1 $6 }
       | 'let' '(' an_ident ':' TypeDef '=' Value ')' { ALet (contentString $3) (-1) $5 $7 $ mkLoc $1 $8 }
       | 'spawn' '{' Call '}'                         { let PCall m k vs _ = $3 in ASpawn m k vs $ mkLoc $1 $4 }
       | '#' ModuleID ':' an_ident '(' ')'            { APrim $2 (contentString $4) [] $ mkLoc $1 $6 }
       | '#' ModuleID ':' an_ident '(' Values ')'     { APrim $2 (contentString $4) $6 $ mkLoc $1 $7 }

TypeAtomic :: { TypeExpr }
TypeAtomic : 'bool'   { TAtom TBool $ tokenLoc $1 }
           | 'int'    { TAtom TInt $ tokenLoc $1 }
           | 'string' { TAtom TString $ tokenLoc $1 }

TypeDef :: { TypeExpr }
TypeDef : 'chan' '<' TypeDef '>' { TChannel $3 $ mkLoc $1 $4 }
        | '(' Types ')'          { TTuple $2 $ mkLoc $1 $3 }
        | TypeAtomic             { $1 }

Types :: { [TypeExpr] }
Types : TypeDef           { [$1] }
      | TypeDef '*' Types { $1:$3 }

Values :: { [Value] }
Values : Value            { [$1] }
       | Value ',' Values { $1:$3 }

Value :: { Value }
Value : 'true'                                   { VTrue (TUnknown noLoc) (tokenLoc $1) }
      | 'false'                                  { VFalse (TUnknown noLoc) (tokenLoc $1) }
      | an_int                                   { VInt (contentInt $1) (TUnknown noLoc) (tokenLoc $1) }
      | a_string                                 { VString (contentString $1) (TUnknown noLoc) (tokenLoc $1) }
      | '(' Values ')'                           { VTuple $2 (TUnknown noLoc) (mkLoc $1 $3) }
      | an_ident                                 { VVar (contentString $1) (TUnknown noLoc) (tokenLoc $1) (-1) }
      | '#' ModuleID ':' an_ident '(' ')'        { VPrim $2 (contentString $4) [] (TUnknown noLoc) (mkLoc $1 $6) }
      | '#' ModuleID ':' an_ident '(' Values ')' { VPrim $2 (contentString $4) $6 (TUnknown noLoc) (mkLoc $1 $7) }

{

parseError :: Token -> Alex a
parseError tok = alexError (show tok)

loc :: Token -> Location
loc tok = tokenLoc tok

lexCont :: (Token -> Alex a) -> Alex a
lexCont cont = do
  t <- alexMonadScan
  cont t

mkLoc' :: Location -> Location -> Location
mkLoc' loc1 loc2 = Location { locOffset      = locOffset loc1
                            , locStartLine   = locStartLine loc1
                            , locStartColumn = locStartColumn loc1
                            , locEndLine     = locEndLine loc2
                            , locEndColumn   = locEndColumn loc2
                            }

mkLoc :: Token -> Token -> Location
mkLoc tok1 tok2 = mkLoc' (tokenLoc tok1) (tokenLoc tok2)

-- | The 'parseModule' function takes a 'String' argument containing a piccolo program
-- and returns either a parsing error, or the corresponding 'Front.AST.Modul'.
parseModule :: String -> Either PiccError Modul
parseModule s = left ParsingError $ runAlex s parse
}
