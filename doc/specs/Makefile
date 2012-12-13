
PDFLATEX=pdflatex

SOURCE=picompil_specs

all: pdf

pdf: $(SOURCE).pdf

$(SOURCE).pdf: $(SOURCE).tex
	$(PDFLATEX) $(SOURCE).tex

clean:
	rm -f *~
	rm -f $(SOURCE).aux
	rm -f $(SOURCE).log
	rm -f $(SOURCE).toc
	rm -f $(SOURCE).out

cleanall: clean
	rm -f $(SOURCE).pdf

