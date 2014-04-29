module Back.SeqAST where

import qualified Front.AST as PilAST

{--
 -
 - Sequential AST types definitions
 -
 - Sequential AST forms an intermediate language to allow output in multiple backend languages.
 - Each type in this file has a phantom type to specify which backend will be used at code generation.
 -
 --}

data Name b
  = Name { name :: String }

data VarName b
  = SimpleName (Name b)               -- name
  | RecordName (VarDescr b) (Name b)  -- name.field
  | ArrayName (VarName b) (Expr b)    -- name[i] or name.field[i]

type VarDescr b = (VarName b,PiccType b)

data PiccType b
  = Sty String                      -- simple type
  | Pty String (PiccType b)         -- parameterized type
  | Fun (PiccType b) [PiccType b]   -- function type

type Value b = (String,PiccType b)

data Expr b
  = Val (Value b)
  | Var (VarDescr b)
  | Op (Binop b) (Expr b) (Expr b)
  | OpU (Unop b) (Expr b)
  | FunCall (VarDescr b) [Expr b]

data Binop b
  = Sum
  | Minus
  | Mult
  | Div
  | Equal

data Unop b
  = Not

data Instr b
  = Comment String
  | Debug String
  | Switch (Expr b) [Case b]
  | SeqBloc [Instr b]                -- instr list handler
  | SemBloc (Instr b)                -- semantic bloc
  | ComBloc PilAST.Location (Instr b)       -- compilation bloc
  | ProcCall (VarDescr b) [Expr b]   -- procedure call
  | DeclareVar (VarDescr b)
  | Assign (VarDescr b) (Expr b)
  | DeclareFun (VarDescr b) [String] (Instr b)
  | ForEach (VarDescr b) (Expr b) (Instr b)
  | If (Expr b) (Instr b) (Instr b)
  | Label String
  | Goto String
  | Return (Expr b)
  | DoWhile (Instr b) (Expr b)

data Case b
  = Case (Expr b) (Instr b)
