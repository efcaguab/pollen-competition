**************************************************************************************

More information about this data set, sampling methods and analysis can be found in:

1. Tur C, Olesen JM & Traveset A. (2014)
   Increasing modularity when downscaling networks from species to individuals.
   Oikos. doi:10.1111/oik.01668

2. Tur C, Vigalondo B, Trøjelsgaard K, Olesen JM & Traveset A. (2014)
   Downscaling pollen-transport networks to the level of individuals.
   Journal of Animal Ecology

Contact e-mail: cris.tur.espinosa@gmail.com

**************************************************************************************


The data package consists of a zip file with the following contents:

R scripts ----------------------------------------------------------------------------

  Rcode_network_analysis_Tur_et_al_2014_Oikos.r
  Rcode_bipartite_weighted_modularity_Tur_et_al_2014_Oikos.r
  Rcode_null_model_construction_Tur_et_al_2014_Oikos.r
  Rcode_comparison_of_modularity_metrics_Tur_et_al_2014_Oikos.r

Data files ---------------------------------------------------------------------------

  binwebCN_sp-sp.txt -- species-species pollen-transport binary matrix for CN site
  binwebCN_i-sp.txt -- individuals-species pollen-transport binary matrix for CN site
  binwebPC_sp-sp.txt -- species-species pollen-transport binary matrix for PC site
  binwebPC_i-sp.txt -- individuals-species pollen-transport binary matrix for PC site

  CNmod1.txt -- module 1 matrix of i-sp network CN site 
  CNmod2.txt -- module 2 matrix of i-sp network CN site 
  CNmod3.txt -- module 3 matrix of i-sp network CN site 
  CNmod4.txt -- module 4 matrix of i-sp network CN site 
  CNmod5.txt -- module 5 matrix of i-sp network CN site 
  CNmod6.txt -- module 6 matrix of i-sp network CN site 

  PCmod1.txt -- module 1 matrix of i-sp network PC site 
  PCmod2.txt -- module 2 matrix of i-sp network PC site  
  PCmod3.txt -- module 3 matrix of i-sp network PC site  
  PCmod4.txt -- module 4 matrix of i-sp network PC site  
  PCmod5.txt -- module 5 matrix of i-sp network PC site  

  ind_prop_list.txt --- column #1  = study site
			column #2  = identification number
			column #3  = specimen label
			column #4  = insect order (HYM: Hymenoptera, DIP: Diptera, COL: Coleoptera, HEM: Hemyptera)
			column #5  = insect species name label
			column #6  = individual code label
			column #7  = capture date
			column #8  = species linkage level
			column #9  = individual linkage level
			column #10 = module membership
			column #11 = among-module connectivity value c
			column #12 = within-module degree z
			column #13 = topological role (P: peripheral, C: connector, MH: module hub, NH: network hub)
			column #14 = individual phenophase (month of capture)
			column #15 = species abundance (number of individuals observed)
			column #16 = species phenophase length (number of weeks)
			column #17 = sex of specimen (m: male, f: female, NA: not available)

  plant_prop_list.txt - column #1  = study site
			column #2  = identification number
			column #3  = plant species or pollen type name label
			column #4  = number of insect individuals on which pollen was found
			column #5  = number of insect species on which pollen was found
			column #6  = among-module connectivity value c
			column #7  = within-module degree z
			column #8  = topological role (P: peripheral, C: connector, MH: module hub, NH: network hub)
			column #9  = module membership
			column #10 = flowering period length in number of weeks
			column #11 = month of maximum flowering peak
			column #12 = pollen abundance (number of pollen grains on insects)

  WIC.TNW_values.txt -- column #1  = insect species name label
			column #2  = study site
			column #3  = degree of individual specialization WIC/TNW
			column #4  = number of individuals captured
			column #5  = species phenophase length (number of days)


  NetCarto_results ---- Empirical_sp-sp_network_CN - Folder with results of unipartite modularity in sp-sp networks CN site
			Empirical_sp-sp_network_PC - Folder with results of unipartite modularity in sp-sp networks PC site
			Empirical_i-sp_network_CN - Folder with results of unipartite modularity in i-sp networks CN site (10 runs of the algorithm)
			Empirical_i-sp_network_PC - Folder with results of unipartite modularity in i-sp networks PC site (10 runs of the algorithm)
			100NULLS_CNi-sp - Folder with results of unipartite modularity in 100 null i-sp networks CN site
			100NULLS_PCi-sp - Folder with results of unipartite modularity in 100 null i-sp networks PC site

  Bipmod_results ------ Empirical_sp-sp_network_CN - Folder with results of bipartite modularity in sp-sp networks CN site
			Empirical_sp-sp_network_PC - Folder with results of bipartite modularity in sp-sp networks PC site
			Empirical_i-sp_network_CN - Folder with results of bipartite modularity i-sp networks CN site (10 runs of the algorithm)
			Empirical_i-sp_network_PC - Folder with results of bipartite modularity i-sp networks PC site (10 runs of the algorithm)
			100NULLS_CNi-sp - Folder with results of bipartite modularity in 100 null i-sp networks CN site
			100NULLS_PCi-sp - Folder with results of bipartite modularity in 100 null i-sp networks PC site

  QuanBiMo_results ---- Empirical_sp-sp_network_CN - Folder with results of weighted bipartite modularity in sp-sp networks CN site
			Empirical_sp-sp_network_PC - Folder with results of weighted bipartite modularity in sp-sp networks PC site
			Empirical_i-sp_network_CN - Folder with results of weighted bipartite modularity i-sp networks CN site (10 runs of the algorithm)
			Empirical_i-sp_network_PC - Folder with results of weighted bipartite modularity i-sp networks PC site (10 runs of the algorithm)
			100null_i-sp_networks - Folder with results of weighted bipartite modularity in 100 null i-sp networks for each study site

