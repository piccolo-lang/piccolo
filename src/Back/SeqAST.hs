{-|
Module         : Back.SeqAST
Description    : The sequential AST for code generation
Stability      : experimental

Piccolo programs are first compiled into sequential AST. This intermediate representation allows
multiple backends modules for further code generation. Note that each type in this module has
a phantom type attached to specify which backend will be used at code generation, since backend
interface in 'Back.Backend' is described in the form of typeclasses.
-}
module Back.SeqAST where

import qualified Front.AST as PilAST

newtype Name b = Name String

-- | Variable name datatype
data VarName b
  = SimpleName (Name b)               -- ^ Simple name
  | RecordName (VarDescr b) (Name b)  -- ^ Fully quelified record name of the form name.field
  | ArrayName (VarName b) (Expr b)    -- ^ Array cell variable of the form name.[epxr]

-- | A variable description is made of a variable name and its type
type VarDescr b = (VarName b,PiccType b)

-- | Types for sequential AST
data PiccType b
  = Sty String                        -- ^ Atomic type
  | Pty String (PiccType b)           -- ^ Parameterized type
  | Fun (PiccType b) [PiccType b]     -- ^ Function type
  deriving (Eq)

-- | Values manipulated by the backend are strings in the generated code, with their types
type Value b = (String,PiccType b)

-- | Sequential AST expressions
data Expr b
  = Val (Value b)                     -- ^  Value
  | Var (VarDescr b)                  -- ^ Variable
  | Op (Binop b) (Expr b) (Expr b)    -- ^ Binary operation applied on two 'Expr'
  | OpU (Unop b) (Expr b)             -- ^ Unary operation applied on an 'Expr'
  | FunCall (VarDescr b) [Expr b]     -- ^ Function call

-- | Binary operations that must be supported by the backend language
data Binop b
  = Sum
  | Minus
  | Mult
  | Div
  | Equal

-- | Unary operations that must be supported by the backend language
data Unop b
  = Not

-- | 'Instr' datatype is the entry point of a sequential AST description
data Instr b
  = Comment String                                -- ^ Comment
  | Debug String                                  -- ^ Debugging information
  | Switch (Expr b) [Instr b]                     -- ^ Switch/case instruction
  | Case (Expr b)                                 -- ^ Case for switch instruction
  | SeqBloc [Instr b]                             -- ^ List of sequential instructions
  | SemBloc [Instr b]                             -- ^ Semantic bloc of instructions
  | ComBloc PilAST.Location (Instr b)             -- ^ Compilation bloc of instructions (represents the compilation of a single piccolo action)
  | ProcCall (VarDescr b) [Expr b]                -- ^ Function call
  | DeclareVar (VarDescr b)                       -- ^ Variable declaration
  | Assign (VarDescr b) (Expr b)                  -- ^ Variable assignation
  | DeclareFun (VarDescr b) [Name b] [Instr b]    -- ^ Function declaration
  | ForEach (VarDescr b) (Expr b) (Instr b)       -- ^ ForEach loop
  | If (Expr b) [Instr b] [Instr b]               -- ^ If instruction
  | Label String                                  -- ^ Label for goto
  | Goto String                                   -- ^ Goto
  | Return (Expr b)                               -- ^ Function return
  | DoWhile (Instr b) (Expr b)                    -- ^ DoWhile loop

