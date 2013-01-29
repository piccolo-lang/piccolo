
OCAMLPATH=$(OCAMLDIR)

DOC=../doc

OCAMLC = $(OCAMLPATH)/ocamlc
OCAMLDOC=$(OCAMLPATH)/ocamldoc
OCFLAGS = -g -annot

YACC = $(OCAMLPATH)/ocamlyacc
LEX = $(OCAMLPATH)/ocamllex

all: stests ttests ctests cctests

Utils: Utils.cmo
PrintUtils: PrintUtils.cmo
Syntax: Syntax.cmo
ASTRepr : ASTRepr.cmo
ASTUtils : ASTUtils.cmo
Types: Types.cmo
TypeRepr: TypeRepr.cmo
PilParser: PilParser.cmo
ParseUtils: ParseUtils.cmo
Typing: Typing.cmo
Middleend: Middleend.cmo
SeqAST: SeqAST.cmo
SeqASTConst: SeqASTConst.cmo
SeqASTConstC: SeqASTConstC.cmo
SeqASTPrettyPrinter: SeqASTPrettyPrinter.cmo
SeqASTPrettyPrinterC: SeqASTPrettyPrinterC.cmo
Backend: Backend.cmo

FICHIERS=$(wildcard *.ml)

doc:
	$(OCAMLDOC) -html -d $(DOC) ${FICHIERS}

Utils.cmo: Utils.ml
	$(OCAMLC) -o Utils.cmo -c $(OCFLAGS) Utils.ml 

PrintUtils.cmo: PrintUtils.ml
	$(OCAMLC) -o $@ -c $(OCFLAGS) $< 

Types.cmo: Types.ml Utils.cmo
	$(OCAMLC) -o Types.cmo -c $(OCFLAGS) Types.ml 

Syntax.cmo: Utils.ml Types.ml Syntax.ml
	$(OCAMLC) -o Syntax.cmo -c $(OCFLAGS) Syntax.ml

ASTRepr.cmo: Utils.ml Syntax.ml Types.ml ASTRepr.ml
	$(OCAMLC) -o ASTRepr.cmo -c $(OCFLAGS) ASTRepr.ml

ASTUtils.cmo: Utils.ml Syntax.ml ASTUtils.ml
	$(OCAMLC) -o ASTUtils.cmo -c $(OCFLAGS) ASTUtils.ml

Middleend.cmo: Utils.ml Syntax.ml ASTUtils.ml Typing.ml Middleend.ml
	$(OCAMLC) -o Middleend.cmo -c $(OCFLAGS) Middleend.ml

SeqAST.cmo : SeqAST.ml
	$(OCAMLC) -o SeqAST.cmo -c $(OCFLAGS) SeqAST.ml


SeqASTConst.cmi: SeqASTConst.mli SeqAST.cmo
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

SeqASTConst.cmo : SeqASTConstGen.ml SeqASTConst.cmi SeqAST.cmo
	rm -f SeqASTConst.ml && ln -s SeqASTConstGen.ml SeqASTConst.ml
	$(OCAMLC) -o $@ -c $(OCFLAGS) SeqASTConst.ml

SeqASTConstC.cmo : SeqASTConstC.ml SeqASTConst.cmi SeqAST.cmo
	rm -f SeqASTConst.ml && ln -s SeqASTConstC.ml SeqASTConst.ml
	$(OCAMLC) -o SeqASTConst.cmo -c $(OCFLAGS) SeqASTConst.ml


SeqASTPrettyPrinter.cmi: SeqASTPrettyPrinter.mli SeqAST.cmo
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

SeqASTPrettyPrinter.cmo: SeqASTPrettyPrinterGen.ml SeqASTPrettyPrinter.cmi SeqAST.cmo PrintUtils.cmo
	rm -f SeqASTPrettyPrinter.ml && ln -s SeqASTPrettyPrinterGen.ml SeqASTPrettyPrinter.ml
	$(OCAMLC) -o $@ -c $(OCFLAGS) SeqASTPrettyPrinter.ml

SeqASTPrettyPrinterC.cmo: SeqASTPrettyPrinterC.ml SeqASTPrettyPrinter.cmi SeqAST.cmo PrintUtils.cmo
	rm -f SeqASTPrettyPrinter.ml && ln -s SeqASTPrettyPrinterC.ml SeqASTPrettyPrinter.ml
	$(OCAMLC) -o SeqASTPrettyPrinter.cmo -c $(OCFLAGS) SeqASTPrettyPrinter.ml


Backend.cmo: Backend.ml Types.cmo Syntax.cmo SeqAST.cmo SeqASTConst.cmi
	$(OCAMLC) -o Backend.cmo -c $(OCFLAGS) Backend.ml 

TypeRepr.cmo: Utils.ml Types.ml TypeRepr.ml
	$(OCAMLC) -o TypeRepr.cmo -c $(OCFLAGS) TypeRepr.ml

PilLexer.cmo: PilParser.cmo

PilParser.cmo: Utils.ml Types.ml TypeRepr.ml Syntax.ml ASTRepr.ml PilParser.mly PilLexer.mll
	$(YACC) -v PilParser.mly
	$(OCAMLC) -c PilParser.mli
	$(LEX) PilLexer.mll
	$(OCAMLC) -c PilLexer.ml -o PilLexer.cmo
	$(OCAMLC) -c PilParser.ml -o PilParser.cmo

ParseUtils.cmo: ParseUtils.ml
	$(OCAMLC) -c $(OCFLAGS) ParseUtils.ml -o ParseUtils.cmo


Typing.cmo: Utils.ml Types.ml TypeRepr.ml Syntax.ml ASTRepr.ml ASTUtils.ml Typing.ml
	$(OCAMLC) -c $(OCFLAGS) Typing.ml -o Typing.cmo

STests.cmo: Utils.ml Types.ml Syntax.ml ASTRepr.ml TypeRepr.ml STests.ml
	$(OCAMLC) -c $(OCFLAGS) STests.ml -o STests.cmo

TTests.cmo: Utils.ml Types.ml Syntax.ml ASTRepr.ml TypeRepr.ml ASTUtils.ml Typing.ml Middleend.ml TTests.ml
	$(OCAMLC) -c $(OCFLAGS) TTests.ml -o TTests.cmo

CTests.cmo: Utils.ml Types.ml Syntax.ml ASTRepr.ml TypeRepr.ml ASTUtils.ml Typing.ml Middleend.ml SeqAST.ml SeqASTConst.mli Backend.ml TTests.ml
	$(OCAMLC) -c $(OCFLAGS) CTests.ml -o CTests.cmo

stests: Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo STests.cmo
	$(OCAMLC) $(OCFLAGS) $^ -o STests

ttests: Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo TTests.cmo
	$(OCAMLC) $(OCFLAGS) $^ -o TTests

ctests: Utils.cmo PrintUtils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo SeqAST.cmo SeqASTConst.cmo SeqASTPrettyPrinter.cmo Backend.cmo CTests.cmo
	$(OCAMLC) $(OCFLAGS) $^ -o CTests

#careful: SeqASTConstC.cmo and SeqASTPrettyPrinterC.cmo in dependances 
#     but SeqASTConst.cmo  and SeqASTPrettyPrinter.cmo in the rule
cctests: Utils.cmo PrintUtils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo SeqAST.cmo SeqASTConstC.cmo SeqASTPrettyPrinterC.cmo Backend.cmo CTests.cmo
	$(OCAMLC) $(OCFLAGS) Utils.cmo PrintUtils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo SeqAST.cmo SeqASTConst.cmo SeqASTPrettyPrinter.cmo Backend.cmo CTests.cmo -o CCTests


clean:
	rm -f *.annot
	rm -f *.cm[oi]
	rm -f SeqASTConst.ml
	rm -f SeqASTPrettyPrinter.ml
	rm -f PilParser.ml
	rm -f PilParser.mli
	rm -f PilParser.output
	rm -f PilLexer.ml
	rm -f *~

cleanall: clean
	rm -f STests
	rm -f TTests
	rm -f CTests
	rm -f CCTests

