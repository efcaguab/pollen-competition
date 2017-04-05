proc_data = ../data/datasets/processed
raw_data = ../data/datasets/raw

all: clean
clean: deposition
deposition: $(proc_data)/deposition/marrero.rds $(proc_data)/deposition/tur.rds

$(proc_data)/deposition/marrero.rds: clean_marrero_dep.R $(raw_data)/marrero2016/estigmatic_pollen.csv
	Rscript $^ $@

$(proc_data)/deposition/tur.rds: clean_tur_dep.R $(raw_data)/tur2016/Data_Tur_et_al_2016_EcolLet.txt $(raw_data)/tur2016/Species_info_Tur_et_al_2016_EcolLet.txt
	Rscript $^ $@
