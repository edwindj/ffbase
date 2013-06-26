MD = $(wildcard *.md)
PDF = $(MD:%.md=%.pdf)

pdf: $(PDF)

%.pdf: %.md
	pandoc -t beamer -V theme:Malmoe $< -o $@
