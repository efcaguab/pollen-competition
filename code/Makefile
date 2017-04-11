proc_data = ../data/datasets/processed
raw_data = ../data/datasets/raw

all: clean
clean: deposition visitation degree
deposition: $(proc_data)/all_deposition.rds
visitation: $(proc_data)/all_visitation.rds
degree: $(proc_data)/all_degree.rds

# deposition

$(proc_data)/deposition/marrero.rds: clean_marrero_dep.R $(raw_data)/marrero2016/estigmatic_pollen.csv
	Rscript $^ $@

$(proc_data)/deposition/tur.rds: clean_tur_dep.R $(raw_data)/tur2016/Data_Tur_et_al_2016_EcolLet.txt $(raw_data)/tur2016/Species_info_Tur_et_al_2016_EcolLet.txt
	Rscript $^ $@

$(proc_data)/deposition/emer.rds: clean_emer_dep.R $(raw_data)/emer2015/pollen_transfer.csv
	Rscript $^ $@

$(proc_data)/all_deposition.rds: assemble.R $(proc_data)/deposition/marrero.rds $(proc_data)/deposition/tur.rds $(proc_data)/deposition/emer.rds
	Rscript $^ $@

# visitation

$(proc_data)/all_visitation.rds: assemble.R $(proc_data)/visitation/marrero.rds
	Rscript $^ $@

$(proc_data)/visitation/marrero.rds: clean_marrero_vis.R $(raw_data)/marrero2016/visitation.csv $(proc_data)/deposition/marrero.rds
	Rscript $^ $@

# degree

$(proc_data)/all_degree.rds: assemble.R $(proc_data)/degree/emer.rds $(proc_data)/degree/marrero.rds
	Rscript $^ $@

$(proc_data)/degree/emer.rds: clean_emer_deg.R $(raw_data)/lopezaraiza2007
	Rscript $^ $@

$(proc_data)/degree/marrero.rds: clean_marrero_deg.R $(proc_data)/visitation/marrero.rds
	Rscript $^ $@
