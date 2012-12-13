
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

Backend.cmo: Types.ml Syntax.ml Backend.ml
	$(OCAMLC) -c $(OCFLAGS) Backend.ml -o Backend.cmo

TypeRepr.cmo: Utils.ml Types.ml TypeRepr.ml
	$(OCAMLC) -c $(OCFLAGS) TypeRepr.ml -o TypeRepr.cmo

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

CTests.cmo: Utils.ml Types.ml Syntax.ml ASTRepr.ml TypeRepr.ml ASTUtils.ml Typing.ml Middleend.ml Backend.ml TTests.ml
	$(OCAMLC) -c $(OCFLAGS) CTests.ml -o CTests.cmo

stests: Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilParser.cmo ParseUtils.cmo STests.cmo
	$(OCAMLC) $(OCFLAGS) Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo STests.cmo -o STests

ttests: Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo TTests.cmo
	$(OCAMLC) $(OCFLAGS) Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo TTests.cmo -o TTests

ctests: Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo Backend.cmo CTests.cmo
	$(OCAMLC) $(OCFLAGS) Utils.cmo Types.cmo TypeRepr.cmo Syntax.cmo ASTRepr.cmo ASTUtils.cmo PilLexer.cmo PilParser.cmo ParseUtils.cmo Typing.cmo Middleend.cmo Backend.cmo CTests.cmo -o CTests

clean:
	rm -f Types.cm[oi]
	rm -f TypeRepr.cm[oi]
	rm -f Syntax.cm[oi]
	rm -f ASTRepr.cm[oi]
	rm -f ASTUtils.cm[oi]
	rm -f Middleend.cm[oi]
	rm -f Utils.cm[oi]
	rm -f PilParser.ml
	rm -f PilParser.mli
	rm -f PilParser.cm[oi]
	rm -f PilParser.output
	rm -f PilLexer.cm[oi]
	rm -f Typing.cm[oi]
	rm -f PilLexer.ml
	rm -f ParseUtils.cm[oi]
	rm -f STests.cm[oi]
	rm -f TTests.cm[oi]
	rm -f *~

cleanall: clean
	rm -f STests
	rm -f TTests


