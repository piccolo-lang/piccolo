
OCAMLPATH=$(OCAMLDIR)

DOC=../doc

OCAMLC = ocamlc
OCAMLDOC= ocamldoc
OCFLAGS = -g -annot

YACC = ocamlyacc
LEX = ocamllex

all: stests ctests ttests ftests pcc

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
Prims : Prims.cmo
PrimitiveUtils : PrimitiveUtils.cmo
SeqAST: SeqAST.cmo
SeqASTConstUtil: SeqASTConstUtil.cmo
SeqASTConstC: SeqASTConstC.cmo
SeqASTPrettyPrinter: SeqASTPrettyPrinter.cmo
SeqASTPrettyPrinterC: SeqASTPrettyPrinterC.cmo
Backend: Backend.cmo
Settings: Settings.cmo
pcc: pcc.cmo

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

Prims.cmo : Prims.ml
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

PrimitiveUtils.cmo : PrimitiveUtils.ml SeqAST.cmi Types.cmi SeqAST.cmi
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

SeqAST.cmi : SeqAST.mli
	$(OCAMLC) -o SeqAST.cmi -c $(OCFLAGS) SeqAST.mli

SeqASTConstGen.cmo : SeqASTConstGen.ml SeqAST.cmi
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

SeqASTConstC.cmo : SeqASTConstC.ml  SeqAST.cmi PrimitiveUtils.cmo
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

SeqASTConstUtil.cmo : SeqASTConstUtil.ml  SeqAST.cmi 
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

SeqASTPrettyPrinterGen.cmo: SeqASTPrettyPrinterGen.ml SeqAST.cmi PrintUtils.ml
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

SeqASTPrettyPrinterC.cmo: SeqASTPrettyPrinterC.ml SeqAST.cmi PrintUtils.cmo Settings.cmo
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<


Backend.cmo: Backend.ml Types.cmo Syntax.cmo SeqASTConstUtil.cmo SeqAST.cmi 
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

TypeRepr.cmo: TypeRepr.ml Utils.ml Types.ml
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

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

Settings.cmo: Settings.ml
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

pcc.cmo: pcc.ml
	$(OCAMLC) -o $@ -c $(OCFLAGS) $<

pcc: Settings.cmo Utils.cmo PrintUtils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo Prims.cmo ParseUtils.cmo PrimitiveUtils.cmo Typing.cmo Middleend.cmo SeqASTConstUtil.cmo SeqASTConstC.cmo SeqASTPrettyPrinterC.cmo Backend.cmo pcc.cmo
	$(OCAMLC) $(OCFLAGS) str.cma $^ -o pcc


STests.cmo: Utils.ml Types.ml Syntax.ml ASTRepr.ml TypeRepr.ml STests.ml
	$(OCAMLC) -c $(OCFLAGS) STests.ml -o STests.cmo

TTests.cmo: Utils.ml Types.ml Syntax.ml ASTRepr.ml TypeRepr.ml ASTUtils.ml Typing.ml Middleend.ml TTests.ml
	$(OCAMLC) -c $(OCFLAGS) TTests.ml -o TTests.cmo

FTests.cmo: Utils.ml Types.ml Syntax.ml ASTRepr.ml TypeRepr.ml ASTUtils.ml Typing.ml Middleend.ml FTests.ml
	$(OCAMLC) -c $(OCFLAGS) FTests.ml -o FTests.cmo

CTests.cmo: Utils.ml Types.ml Syntax.ml ASTRepr.ml TypeRepr.ml ASTUtils.ml Typing.ml Middleend.ml Backend.ml CTests.ml
	$(OCAMLC) -c $(OCFLAGS) CTests.ml -o CTests.cmo

stests: Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo STests.cmo
	$(OCAMLC) $(OCFLAGS) $^ -o STests

ttests: Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo TTests.cmo
	$(OCAMLC) $(OCFLAGS) $^ -o TTests

ftests: Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo FTests.cmo
	$(OCAMLC) $(OCFLAGS) $^ -o FTests

ctests: Settings.cmo Utils.cmo PrintUtils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo Prims.cmo ParseUtils.cmo Typing.cmo PrimitiveUtils.cmo Middleend.cmo SeqASTConstUtil.cmo SeqASTConstC.cmo SeqASTPrettyPrinterC.cmo SeqASTConstGen.cmo SeqASTPrettyPrinterGen.cmo Backend.cmo CTests.cmo
	$(OCAMLC) $(OCFLAGS) str.cma $^ -o CTests



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
	rm -f FTests
	rm -f pcc
