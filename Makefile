

all: outline.pdf

clean:
	rm *.pdf

%.pdf: %.md beamer.tex
	pandoc -t beamer -V fontsize=10pt --template=beamer.tex -o $@ $<
