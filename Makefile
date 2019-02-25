.PHONY: run_main

run_main:
	Rscript main.R

abc_figures:
	Rscript ./presentations/abc2018/make_figures.R

release:
	docker save pollen-competition > docker-image.tar \
	&& zip -sd manuscript-files paper/*.pdf paper/*.tex /paper*.aux;
	zip -r -sd -b paper manuscript-files . -i *_files/*
