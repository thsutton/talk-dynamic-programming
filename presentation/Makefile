
PANDOC := pandoc -t beamer -V listings -V tables -V url -V graphics -V fontsize:10pt -V classoption:compress --template=beamer.tex

TARGET := slides

all: outline-16x10.pdf outline-16x9.pdf outline-14x9.pdf  outline-5x4.pdf outline-4x3.pdf outline-3x2.pdf

clean:
	rm -f *.pdf
	rm -f *.aux *.log

%.pdf: %.dot
	dot -Tpdf -o $@ $<

%.pdf: %.tex
	pdflatex $<

%.pdf: %.md beamer.tex
	$(PANDOC) -o $@ $<

%-16x10.pdf: %.md outline.pdf beamer.tex
	$(PANDOC) -V classoption:aspectratio=1610 -o $@ $<

%-16x9.pdf: %.md outline.pdf beamer.tex
	$(PANDOC) -V classoption:aspectratio=169 -o $@ $<

%-14x9.pdf: %.md outline.pdf beamer.tex
	$(PANDOC) -V classoption:aspectratio=149 -o $@ $<

%-5x4.pdf: %.md outline.pdf beamer.tex
	$(PANDOC) -V classoption:aspectratio=54 -o $@ $<

%-4x3.pdf: %.md outline.pdf beamer.tex
	$(PANDOC) -V classoption:aspectratio=43 -o $@ $<

%-3x2.pdf: %.md outline.pdf beamer.tex
	$(PANDOC) -V classoption:aspectratio=32 -o $@ $<
