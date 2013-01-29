
val print_piccType : Format.formatter -> SeqAST.piccType -> unit

val print_binop : Format.formatter -> SeqAST.binop -> unit

val print_varName : Format.formatter -> SeqAST.varName -> unit

val print_expr : Format.formatter -> SeqAST.expr -> unit

val print_instr : Format.formatter -> SeqAST.instr -> unit

val print_instr_list_std : SeqAST.instr list -> unit
