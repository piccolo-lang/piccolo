
OCAMLPATH=$(OCAMLDIR)

DOC=../doc

OCAMLC = $(OCAMLPATH)/ocamlc
OCAMLDOC=$(OCAMLPATH)/ocamldoc
OCFLAGS = -g

YACC = $(OCAMLPATH)/ocamlyacc
LEX = $(OCAMLPATH)/ocamllex

all: stests ttests ctests

Utils: Utils.cmo
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
Backend: Backend.cmo

FICHIERS=$(wildcard *.ml)

doc:
	$(OCAMLDOC) -html -d $(DOC) ${FICHIERS}

Utils.cmo: Utils.ml
	$(OCAMLC) -c $(OCFLAGS) Utils.ml -o Utils.cmo

Types.cmo: Utils.ml Types.ml
	$(OCAMLC) -c $(OCFLAGS) Types.ml -o Types.cmo

Syntax.cmo: Utils.ml Types.ml Syntax.ml
	$(OCAMLC) -c $(OCFLAGS) Syntax.ml -o Syntax.cmo

ASTRepr.cmo: Utils.ml Syntax.ml Types.ml ASTRepr.ml
	$(OCAMLC) -c $(OCFLAGS) ASTRepr.ml -o ASTRepr.cmo

ASTUtils.cmo: Utils.ml Syntax.ml ASTUtils.ml
	$(OCAMLC) -c $(OCFLAGS) ASTUtils.ml -o ASTUtils.cmo

Middleend.cmo: Utils.ml Syntax.ml ASTUtils.ml Typing.ml Middleend.ml
	$(OCAMLC) -c $(OCFLAGS) Middleend.ml -o Middleend.cmo

SeqAST.cmo : SeqAST.ml
	$(OCAMLC) -c $(OCFLAGS) SeqAST.ml -o SeqAST.cmo

SeqASTConst.cmi: SeqASTConst.mli
	$(OCAMLC) -c $(OCFLAGS) SeqASTConst.mli -o SeqASTConst.cmi

SeqASTConst.cmo : SeqASTConst.cmi SeqASTConst.ml SeqAST.ml
	$(OCAMLC) -c $(OCFLAGS) SeqASTConst.ml -o SeqASTConst.cmo

SeqASTConstC.cmo : SeqASTConst.cmi SeqASTConstC.ml SeqAST.ml
	$(OCAMLC) -c $(OCFLAGS) SeqASTConstC.ml -o SeqASTConst.cmo

SeqASTPrettyPrinter.cmo: SeqASTPrettyPrinter.ml SeqAST.ml
	$(OCAMLC) -c $(OCFLAGS) $< -o $@

Backend.cmo: Types.ml Syntax.ml SeqAST.ml SeqASTConst.cmi Backend.ml
	$(OCAMLC) -c $(OCFLAGS) Backend.ml -o Backend.cmo

TypeRepr.cmo: Utils.ml Types.ml TypeRepr.ml
	$(OCAMLC) -c $(OCFLAGS) TypeRepr.ml -o TypeRepr.cmo

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

ctests: Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo SeqAST.cmo SeqASTConst.cmo SeqASTPrettyPrinter.cmo Backend.cmo CTests.cmo
	$(OCAMLC) $(OCFLAGS) $^ -o CTests

cctests: Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo SeqAST.cmo SeqASTConstC.cmo SeqASTPrettyPrinter.cmo Backend.cmo CTests.cmo
	$(OCAMLC) $(OCFLAGS) Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo SeqAST.cmo SeqASTConst.cmo SeqASTPrettyPrinter.cmo Backend.cmo CTests.cmo -o CCTests


clean:
	rm -f *.cm[oi]
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

