
PANDOC := pandoc -t beamer -V fontsize:10pt -V toc --template=beamer.tex

all: outline.pdf

clean:
	rm *.pdf

%.pdf: %.md beamer.tex
	$(PANDOC) -o $@ $<

%-handout.pdf: %.md beamer.tex
	$(PANDOC) -V handout:handout -o $@ $<
