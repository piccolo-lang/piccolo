module Middle.Compilation (compilePass) where

import PiccError
import Front.AST
import Back.SeqAST

compilePass :: ModuleDef -> Either PiccError (Instr a)
compilePass = error "TODO Middle.Compilation.compilePass"

