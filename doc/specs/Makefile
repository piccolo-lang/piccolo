
PDFLATEX=pdflatex

COMPILE_SPEC=picompil_specs
DATAREPR_SPEC=datarepr_specs

all: compile datarepr

compile: $(COMPILE_SPEC).pdf

datarepr: $(DATAREPR_SPEC).pdf

$(COMPILE_SPEC).pdf: $(COMPILE_SPEC).tex
	$(PDFLATEX) $(COMPILE_SPEC).tex

$(DATAREPR_SPEC).pdf: $(DATAREPR_SPEC).tex
	$(PDFLATEX) $(DATAREPR_SPEC).tex


clean:
	rm -f *~
	rm -f $(COMPILE_SPEC).aux
	rm -f $(COMPILE_SPEC).log
	rm -f $(COMPILE_SPEC).toc
	rm -f $(COMPILE_SPEC).out
	rm -f $(DATAREPR_SPEC).aux
	rm -f $(DATAREPR_SPEC).log
	rm -f $(DATAREPR_SPEC).toc
	rm -f $(DATAREPR_SPEC).out

cleanall: clean
	rm -f $(COMPILE_SPEC).pdf
	rm -f $(DATAREPR_SPEC).pdf

