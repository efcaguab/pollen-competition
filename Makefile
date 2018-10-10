.PHONY: run_main

run_main:
	Rscript main.R

LOGFILE=`date +'%F'`

draft:  
	zip -j $(LOGFILE) paper/*.pdf

abc_figures:
	Rscript ./presentations/abc2018/make_figures.R