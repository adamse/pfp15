repa.pdf: intro.tex repa.tex
	latexmk -pdf repa.tex

intro.tex:
	pandoc intro.md -t latex > intro.tex

repa.tex:
	lhs2tex filter.lhs > repa.tex
