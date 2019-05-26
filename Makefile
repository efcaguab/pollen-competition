.PHONY: run_main

run_main:
	Rscript main.R

abc_figures:
	Rscript ./presentations/abc2018/make_figures.R

release:
	zip -sd manuscript-files paper/*.pdf paper/*.tex paper/bibliography.bib /paper/*.aux /paper/*.docx;
	zip -r -sd -b paper manuscript-files . -i *_files/*

one_pdf: run_main stich_pdf_draft

stich_pdf_manuscript:
	pdftk paper/manuscript.pdf paper/supp-info.pdf cat output draft.pdf

stich_pdf_draft:
	pdftk paper/draft-info.pdf paper/cover-letter.pdf paper/manuscript.pdf paper/supp-info.pdf cat output draft.pdf
