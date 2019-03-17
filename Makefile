.PHONY: run_main

run_main:
	Rscript main.R

abc_figures:
	Rscript ./presentations/abc2018/make_figures.R

release:
	docker save pollen-competition > docker-image.tar \
	&& zip -sd manuscript-files paper/*.pdf paper/*.tex /paper/*.aux /paper/*.docx;
	zip -r -sd -b paper manuscript-files . -i *_files/*

one_pdf: run_main stich_pdf

stich_pdf:
	pdftk paper/draft-info.pdf paper/manuscript.pdf paper/supp-info.pdf cat output draft.pdf
