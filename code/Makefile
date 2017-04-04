proc_data = ../data/datasets/processed
raw_data = ../data/datasets/raw

$(proc_data)/deposition/marrero.rds: clean_marrero_dep.R $(raw_data)/marrero2016/estigmatic_pollen.csv
	Rscript $^ $@
