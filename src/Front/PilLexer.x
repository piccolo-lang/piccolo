{
module Front.PilLexer where

import Front.AST
import Utils.Location
}

%wrapper "monad"

$digit   = [0-9]
$alpha   = [A-Za-z]
$idchars = [_$digit$alpha]
$quote   = [\']
@int     = [1-9][0-9]*
@str     = \"[^\"]*\"
@ident   = $alpha $idchars* $quote*

tokens :-
  $white+                                      ;
  "--".*                                       ;
  $digit                                       { tok (\s l -> TokenVInt l (read s)) }
  @int                                         { tok (\s l -> TokenVInt l (read s)) }
  @str                                         { tok $ flip TokenVString   }
  true                                         { tok' $ flip TokenVBool True }
  false                                        { tok' $ flip TokenVBool False   }
  module                                       { tok' TokenModule          }
  def                                          { tok' TokenDef             }
  end                                          { tok' TokenEnd             }
  tau                                          { tok' TokenTau             }
  new                                          { tok' TokenNew             }
  spawn                                        { tok' TokenSpawn           }
  let                                          { tok' TokenLet             }
  chan                                         { tok' TokenChan            }
  bool                                         { tok' TokenBool            }
  int                                          { tok' TokenInt             }
  string                                       { tok' TokenString          }
  \<                                           { tok' TokenInf             }
  \>                                           { tok' TokenSup             }
  \(                                           { tok' TokenLParen          }
  \)                                           { tok' TokenRParen          }
  \[                                           { tok' TokenLBrack          }
  \]                                           { tok' TokenRBrack          }
  \{                                           { tok' TokenLCurly          }
  \}                                           { tok' TokenRCurly          }
  \,                                           { tok' TokenComma           }
  \/                                           { tok' TokenSlash           }
  \=                                           { tok' TokenEq              }
  \:                                           { tok' TokenColon           }
  \+                                           { tok' TokenPlus            }
  \!                                           { tok' TokenOut             }
  \?                                           { tok' TokenIn              }
  \#                                           { tok' TokenSharp           }
  \*                                           { tok' TokenStar            }
  @ident                                       { tok $ flip TokenIdent     }

{

data Token = TokenVInt    { tokenLoc :: Location, contentInt :: Int }
           | TokenVString { tokenLoc :: Location, contentString :: String }
           | TokenVBool   { tokenLoc :: Location, contentBool :: Bool }
           | TokenModule  { tokenLoc :: Location }
           | TokenDef     { tokenLoc :: Location }
           | TokenEnd     { tokenLoc :: Location }
           | TokenTau     { tokenLoc :: Location }
           | TokenNew     { tokenLoc :: Location }
           | TokenSpawn   { tokenLoc :: Location }
           | TokenLet     { tokenLoc :: Location }
           | TokenChan    { tokenLoc :: Location }
           | TokenBool    { tokenLoc :: Location }
           | TokenInt     { tokenLoc :: Location }
           | TokenString  { tokenLoc :: Location }
           | TokenInf     { tokenLoc :: Location }
           | TokenSup     { tokenLoc :: Location }
           | TokenLParen  { tokenLoc :: Location }
           | TokenRParen  { tokenLoc :: Location }
           | TokenLBrack  { tokenLoc :: Location }
           | TokenRBrack  { tokenLoc :: Location }
           | TokenLCurly  { tokenLoc :: Location }
           | TokenRCurly  { tokenLoc :: Location }
           | TokenComma   { tokenLoc :: Location }
           | TokenSlash   { tokenLoc :: Location }
           | TokenEq      { tokenLoc :: Location }
           | TokenColon   { tokenLoc :: Location }
           | TokenPlus    { tokenLoc :: Location }
           | TokenOut     { tokenLoc :: Location }
           | TokenIn      { tokenLoc :: Location }
           | TokenSharp   { tokenLoc :: Location }
           | TokenStar    { tokenLoc :: Location }
           | TokenIdent   { tokenLoc :: Location, contentString :: String }
           | TokenEOF     { tokenLoc :: Location }
           deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = return $ TokenEOF (Location 0 0 0 0 0)

tok :: (String -> Location -> a) -> AlexInput -> Int -> Alex a
tok f loc@(p@(AlexPn offset line column),_,_,input) len = do
  let loc = Location { locOffset      = offset
                     , locStartLine   = line
                     , locStartColumn = column
                     , locEndLine     = line
                     , locEndColumn   = column + len
                     }
  return $ f (take len input) loc

tok' :: (Location -> a) -> AlexInput -> Int -> Alex a
tok' = tok . const

}

