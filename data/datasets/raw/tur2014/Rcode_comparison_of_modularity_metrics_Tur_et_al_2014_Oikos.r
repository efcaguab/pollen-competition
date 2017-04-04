#########################################################################################################################
## R code for analysis included in:
## Tur C, Olesen JM & Traveset A. (2014)
## Increasing modularity when downscaling networks from species to individuals
## Oikos
#########################################################################################################################


# UNIPARTITE MODULARITY RESULTS ###########################################################################################################
## Netcarto program: http://etseq.urv.cat/seeslab/downloads/network-cartography-netcarto/
## Reference: Guimerà R. & Amaral LAN (2005) http://dx.doi.org/10.1088/1742-5468/2005/02/P02001
###########################################################################################################################################

# Before starting set the working directory in the folder with the downloaded scripts and data --------------------------------------------

  workdir<-getwd() # save name of path

# Extract output from NetCarto program for 100 null i-sp networks (results of modularity value and number of modules)  --------------------

  n<-c(1:100)
  null.run<-c()
  for(i in 1:length(n)){
      null.run[i]<-paste("Run_null_",n[i],sep="") # create a vector with the name of the folders for each null model
  }
  null.run
  nmod.cn<-c() # CN Site
  mod.files<-list()
  mod.netcarto.cn<-list()
  unimod.null.cn<-c()
  for(i in 1:length(null.run)){
      setwd(paste(workdir,"/NetCarto_results/100NULLS_CNi-sp/",null.run[i],sep="")) # Important!: Folders with results files must be inside the working directory
      mod.files[[i]]<-read.table("modules.dat", header =F, sep="",dec=".",fill=T)
      nmod.cn[i]<-nrow(mod.files[[i]])
      mod.netcarto.cn[[i]]<-read.table("randomized_mod.dat", header =F, sep="",dec=".",fill=T)
      unimod.null.cn[i]<-mod.netcarto.cn[[i]][1]
  }
  unimod.null.cn<-sapply(unimod.null.cn,mean)
  nmod.cn
  mean(nmod.cn);sd(nmod.cn) # Number of modules obtained with NetCarto (mean and sd) 100 nulls i-sp networks CN site
  mean(unimod.null.cn);sd(unimod.null.cn) # Mean unipartite modularity for CN site 100 nulls i-sp networks CN site

  nmod.pc<-c() # PC site
  mod.files<-list()
  mod.netcarto.pc<-list()
  unimod.null.pc<-c()
  for(i in 1:length(null.run)){
      setwd(paste(workdir,"/NetCarto_results/100NULLS_PCi-sp/",null.run[i],sep=""))
      mod.files[[i]]<-read.table("modules.dat", header =F, sep="",dec=".",fill=T)
      nmod.pc[i]<-nrow(mod.files[[i]])
      mod.netcarto.pc[[i]]<-read.table("randomized_mod.dat", header =F, sep="",dec=".",fill=T)
      unimod.null.pc[i]<-mod.netcarto.pc[[i]][1]
  }
  unimod.null.pc<-sapply(unimod.null.pc,mean)
  nmod.pc
  mean(nmod.pc);sd(nmod.pc) # Number of modules obtained with NetCarto (Mean +/- sd) 100 nulls i-sp networks PC site
  mean(unimod.null.pc);sd(unimod.null.pc) # Mean unipartite modularity for CN site 100 nulls i-sp networks PC site

# Extract results (modularity value and number of modules) from NetCarto output for empirical i-sp networks (10 runs) ------------

  n<-c(1:10)
  run<-c()
  for(i in 1:length(n)){
  run[i]<-paste("Run_",n[i],sep="") # create a vector with the name of the folders for each run of the empirical networks (10 runs)
  }
  run
  unimod.emp.cn.i<-list()
  unimod.emp.pc.i<-list()
  for(i in 1:length(run)){
    setwd(paste(workdir,"/NetCarto_results/Empirical_i-sp_network_CN/",run[i],sep=""))
    unimod.emp.cn.i[[i]]<-read.table("randomized_mod.dat", header =F, sep="",dec=".",fill=T)
    unimod.emp.cn.i[i]<-unimod.emp.cn.i[[i]][1]
    setwd(paste(workdir,"/NetCarto_results/Empirical_i-sp_network_PC/",run[i],sep=""))
    unimod.emp.pc.i[[i]]<-read.table("randomized_mod.dat", header =F, sep="",dec=".",fill=T)
    unimod.emp.pc.i[i]<-unimod.emp.pc.i[[i]][1]
  }
  mean(sapply(unimod.emp.cn.i,mean)) # Unipartite modularity of empirical i-sp network (mean of 10 runs of the algorithm) CN site
  mean(sapply(unimod.emp.pc.i,mean)) # Unipartite modularity of empirical i-sp network (mean of 10 runs of the algorithm) PC site

# Prepare a dataframe with module members and module number assigned by NetCarto in different 10 runs ------------------------------------------------------------

  n<-c(1:10)
  run<-c()
  for(i in 1:length(n)){
  run[i]<-paste("Run_",n[i],sep="") # create a vector with the name of the folders for each run of the empirical matrices
  }
  run
  netcarto.nmod.cn<-c()
  netcarto.nmod.pc<-c()
  netcarto.mod.files.cn<-list()
  netcarto.mod.files.pc<-list()
  for(i in 1:length(run)){
    setwd(paste(workdir,"/NetCarto_results/Empirical_i-sp_network_CN/",run[i],sep=""))
      netcarto.mod.files.cn[[i]]<-read.table("modules.dat", header =F, sep="",dec=".",fill=T)
      netcarto.nmod.cn[i]<-nrow(netcarto.mod.files.cn[[i]])
    setwd(paste(workdir,"/NetCarto_results/Empirical_i-sp_network_PC/",run[i],sep=""))
      netcarto.mod.files.pc[[i]]<-read.table("modules.dat", header =F, sep="",dec=".",fill=T)
      netcarto.nmod.pc[i]<-nrow(netcarto.mod.files.pc[[i]])
  }
  mean(netcarto.nmod.cn);sd(netcarto.nmod.cn) # Mean number of modules obtained in 10 runs with NetCarto in i-sp network CN site
  mean(netcarto.nmod.pc);sd(netcarto.nmod.pc) # Mean number of modules obtained in 10 runs with NetCarto in i-sp network PC site

  netcarto.mod.files.cn
  runs.cn<-list()
  for(j in 1:length(netcarto.mod.files.cn)){
      mod.members<-list()
      mod.number<-list()
      for(i in 1:dim(netcarto.mod.files.cn[[j]])[1]){
          mod.members[[i]]<-as.vector(as.matrix(netcarto.mod.files.cn[[j]][i,c(10:dim(netcarto.mod.files.cn[[j]])[2])]))
          mod.members[[i]]<-mod.members[[i]][!is.na(mod.members[[i]])]
          mod.number[[i]]<-rep(i,length(mod.members[[i]]))
      }
      members<-unlist(mod.members)
      modules<-unlist(mod.number)
      data<-data.frame(members,modules)
      runs.cn[[j]]<-data[order(data$members),]
  }
  runs.cn # list with the partitions obtained in different runs
  nruns<-do.call(cbind,runs.cn)
  mod.runs.netcarto.cn<-data.frame(nodes=nruns[,1],nruns[,grep("modules",colnames(nruns))])# extract module membership of each run and put it into a dataframe
  names(mod.runs.netcarto.cn)<-c("nodes",paste(rep("NetCarto",length(run)),run,sep="_"))
  mod.runs.netcarto.cn
  mod.runs.netcarto.cn<-rbind(mod.runs.netcarto.cn,c(25,rep(7,10)),c(224,rep(7,10)))# add isolated nodes 25 and 224
  mod.runs.netcarto.cn<-mod.runs.netcarto.cn[order(mod.runs.netcarto.cn$nodes),]
  mod.runs.netcarto.cn # Dataframe with module partitions obtained with Netcarto for i-sp network CN site

  netcarto.mod.files.pc
  runs.pc<-list()
  for(j in 1:length(netcarto.mod.files.pc)){
      mod.members<-list()
      mod.number<-list()
      for(i in 1:dim(netcarto.mod.files.pc[[j]])[1]){
          mod.members[[i]]<-as.vector(as.matrix(netcarto.mod.files.pc[[j]][i,c(10:dim(netcarto.mod.files.pc[[j]])[2])]))
          mod.members[[i]]<-mod.members[[i]][!is.na(mod.members[[i]])]
          mod.number[[i]]<-rep(i,length(mod.members[[i]]))
      }
      members<-unlist(mod.members)
      modules<-unlist(mod.number)
      data<-data.frame(members,modules)
      runs.pc[[j]]<-data[order(data$members),]
  }
  runs.pc # list with the partitions obtained in different runs
  nruns<-do.call(cbind,runs.pc)
  mod.runs.netcarto.pc<-data.frame(nodes=nruns[,1],nruns[,grep("modules",colnames(nruns))])# extract module membership of each run and put it into a dataframe
  names(mod.runs.netcarto.pc)<-c("nodes",paste(rep("NetCarto",length(run)),run,sep="_"))
  mod.runs.netcarto.pc<-mod.runs.netcarto.pc[order(mod.runs.netcarto.pc$nodes),]
  mod.runs.netcarto.pc # Dataframe with module partitions obtained with Netcarto for i-sp network PC site

  
# BIPARTITE MODULARITY RESULTS #############################################################################################################
## BIPMOD program: http://onlinelibrary.wiley.com/doi/10.1111/jbi.12015/suppinfo
## Reference: Thébault (2014) http://onlinelibrary.wiley.com/doi/10.1111/jbi.12015/pdf
############################################################################################################################################

# Extract the modularity values obtained for 100 null i-sp networks with BIPMOD program -----------------------------------------------------

  n<-c(1:100)
  null.run<-c()
  for(i in 1:length(n)){
      null.run[i]<-paste("Run_null_",n[i],sep="") # create a vector with the name of the folders for each null model
  }
  null.run
  bipmod.null.files.cn<-list()
  bipmod.null.files.pc<-list()
  bipmod.null.cn<-c()
  bipmod.null.pc<-c()
  nbipmod.null.cn<-c()
  nbipmod.null.pc<-c()
  for(i in 1:length(null.run)){
      setwd(paste(workdir,"/Bipmod_results/100NULLS_CNi-sp/",null.run[i],sep="")) # Important!: Folders with results files must be inside the working directory
      bipmod.null.files.cn[[i]]<-read.table("modularity.txt", header =T, sep="\t",dec=".",fill=T)
      bipmod.null.cn[i]<-bipmod.null.files.cn[[i]][1,2]
      nbipmod.null.cn[i]<-bipmod.null.files.cn[[i]][1,1]
      setwd(paste(workdir,"/Bipmod_results/100NULLS_PCi-sp/",null.run[i],sep=""))
      bipmod.null.files.pc[[i]]<-read.table("modularity.txt", header =T, sep="\t",dec=".",fill=T)
      bipmod.null.pc[i]<-bipmod.null.files.pc[[i]][1,2]
      nbipmod.null.pc[i]<-bipmod.null.files.pc[[i]][1,1]
  }
  bipmod.null.cn # bipartite modularity values for 100 null i-sp networks CN site
  bipmod.null.pc # bipartite modularity values for 100 null i-sp networks PC site
  mean(bipmod.null.cn); sd(bipmod.null.cn) # mean and sd bipartite modularity of 100 null i-sp networks CN site
  mean(bipmod.null.pc); sd(bipmod.null.pc) # mean and sd bipartite modularity of 100 null i-sp networks PC site
  mean(nbipmod.null.cn); sd(nbipmod.null.cn) # mean and sd number of modules with bipartite modularity of 100 null i-sp networks CN site
  mean(nbipmod.null.pc); sd(nbipmod.null.pc) # mean and sd number of modules with bipartite modularity of 100 null i-sp networks PC site

# Extract the modules and average modularity obtained in 10 runs with BIPMOD program for empirical i-sp networks -----------------------

  n<-c(1:10)
  run<-c()
  for(i in 1:length(n)){
  run[i]<-paste("Run_",n[i],sep="")
  }
  run
  nmod.bipmod.cn<-c()
  mod.files.bipmod.cn<-list()
  nmod.bipmod.pc<-c()
  mod.files.bipmod.pc<-list()
  for(i in 1:length(run)){
      setwd(paste(workdir,"/Bipmod_results/Empirical_i-sp_network_CN/",run[i],sep=""))
      mod.files.bipmod.cn[[i]]<-read.table("species_compt.txt", header =F, sep="\t",dec=".",fill=T)
      mod.files.bipmod.cn[[i]]<-mod.files.bipmod.cn[[i]][c(2,4),]
      mod.files.bipmod.cn[[i]]<-cbind(mod.files.bipmod.cn[[i]][1,],mod.files.bipmod.cn[[i]][2,])
      mod.files.bipmod.cn[[i]]<-mod.files.bipmod.cn[[i]][!is.na(mod.files.bipmod.cn[[i]])]
      mod.files.bipmod.cn[[i]]<-as.integer(mod.files.bipmod.cn[[i]])
      nmod.bipmod.cn[i]<-length(unique(mod.files.bipmod.cn[[i]]))
      setwd(paste(workdir,"/Bipmod_results/Empirical_i-sp_network_PC/",run[i],sep=""))
      mod.files.bipmod.pc[[i]]<-read.table("species_compt.txt", header =F, sep="\t",dec=".",fill=T)
      mod.files.bipmod.pc[[i]]<-mod.files.bipmod.pc[[i]][c(2,4),]
      mod.files.bipmod.pc[[i]]<-cbind(mod.files.bipmod.pc[[i]][1,],mod.files.bipmod.pc[[i]][2,])
      mod.files.bipmod.pc[[i]]<-mod.files.bipmod.pc[[i]][!is.na(mod.files.bipmod.pc[[i]])]
      mod.files.bipmod.pc[[i]]<-as.integer(mod.files.bipmod.pc[[i]])
      nmod.bipmod.pc[i]<-length(unique(mod.files.bipmod.pc[[i]]))
  }
  mean(nmod.bipmod.cn);sd(nmod.bipmod.cn) # Mean number of modules with Bipmod 10 runs i-sp CN site
  mean(nmod.bipmod.pc);sd(nmod.bipmod.pc) # Mean number of modules with Bipmod 10 runs i-sp PC site

  mod.files.bipmod.cn # list with module membership of nodes in the different runs of bipmod
  mod.bipmod.cn<-do.call(cbind,mod.files.bipmod.cn)
  colnames(mod.bipmod.cn)<-c(paste(rep("Bipmod",length(run)),run,sep="_"))
  mod.bipmod.cn # we can't have 0 as a module id, so we need to replace all 0
  for(i in 1:ncol(mod.bipmod.cn)){
  mod.bipmod.cn[,i]<-as.factor(mod.bipmod.cn[,i])
  levels(mod.bipmod.cn[,i])<-c(1:length(unique(mod.bipmod.cn[,i])))
  }
  mod.bipmod.cn
  mod.bipmod.cn<-data.frame(nodes=c(1:245),mod.bipmod.cn)
  mod.files.bipmod.pc
  mod.bipmod.pc<-do.call(cbind,mod.files.bipmod.pc)
  colnames(mod.bipmod.pc)<-c(paste(rep("Bipmod",length(run)),run,sep="_"))
  mod.bipmod.pc # we can't have 0 as a module id, so we need to replace all 0
  for(i in 1:ncol(mod.bipmod.pc)){
  mod.bipmod.pc[,i]<-as.factor(mod.bipmod.pc[,i])
  levels(mod.bipmod.pc[,i])<-c(1:length(unique(mod.bipmod.pc[,i])))
  }
  mod.bipmod.pc
  mod.bipmod.pc<-data.frame(nodes=c(1:186),mod.bipmod.pc)
  mod.bipmod.cn # Dataframe of module partitions obtained with bipmod for CN site (10 runs)
  mod.bipmod.pc # Dataframe of module partitions obtained with bipmod for PC site (10 runs)
  
  n<-c(1:10)
  run<-c()
  for(i in 1:length(n)){
  run[i]<-paste("Run_",n[i],sep="")
  }
  run
  bipmod.files.values.cn<-list()
  bipmod.files.values.pc<-list()
  bipmod.values.cn<-c()
  bipmod.values.pc<-c()
  for(i in 1:length(run)){
      setwd(paste(workdir,"/Bipmod_results/Empirical_i-sp_network_CN/",run[i],sep=""))
      bipmod.files.values.cn[[i]]<-read.table("modularity.txt", header =T, sep="\t",dec=".",fill=T)
      bipmod.values.cn[i]<-bipmod.files.values.cn[[i]][1,2]
      setwd(paste(workdir,"/Bipmod_results/Empirical_i-sp_network_PC/",run[i],sep=""))
      bipmod.files.values.pc[[i]]<-read.table("modularity.txt", header =T, sep="\t",dec=".",fill=T)
      bipmod.values.pc[i]<-bipmod.files.values.pc[[i]][1,2]
  }
  bipmod.values.cn # bipartite modularity values for 10 runs of i-sp networks CN site
  bipmod.values.pc # bipartite modularity values for 10 runs of i-sp networks PC site
  mean(bipmod.values.cn); sd(bipmod.values.cn) # mean and sd bipartite modularity of 10 runs of i-sp networks CN site
  mean(bipmod.values.pc); sd(bipmod.values.pc) # mean and sd bipartite modularity of 10 runs of i-sp networks PC site

  
# WEIGHTED BIPARTITE MODULARITY RESULTS ############################################################################################################
# Weighted bipartite modularity (QuanBiMo) is implemented in function computeModules in package bipartite.
# See file 'Rcode_bipartite_weighted_modularity.r' included in this data package for calculating weighted bipartite modularity in R.
# Reference: Dormann & Strauss(2014) http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12139/pdf
####################################################################################################################################################

# Extract the modularity values of 100 runs with QuanBiMo ------------------------------------------------------------------------------------------

  setwd(paste(workdir,"/QuanBiMo_results/100null_i-sp_networks",sep=""))
  wbipmod.null.cn<-read.table("Qvalues_i-sp_100nulls.CN.txt", header =T, sep=" ",dec=".")
  wbipmod.null.pc<-read.table("Qvalues_i-sp_100nulls.PC.txt", header =T, sep=" ",dec=".")
  wbipmod.null.cn # weighted modularity values for 100 i-sp networks CN site
  wbipmod.null.pc # weighted modularity values for 100 i-sp networks PC site
  mean(wbipmod.null.cn[,2]);sd(wbipmod.null.cn[,2]) # mean and sd weighted bipatite modularity of null i-sp networks CN site
  mean(wbipmod.null.pc[,2]);sd(wbipmod.null.pc[,2]) # mean and sd weighted bipatite modularity of null i-sp networks PC site

# Extract the modularity values obtained with 10 runs of QuanBiMo for empirical i-sp networks -----------------------------------------------------

  setwd(paste(workdir,"/QuanBiMo_results/Empirical_i-sp_network_CN/10_runs",sep=""))
  wbipmod.cn<-read.table("Qvalues_i-spCN_10runs.txt", header =T, sep=",",dec=".")
  wbipmod.cn # weighted modularity values for 10 runs i-sp networks CN site
  setwd(paste(workdir,"/QuanBiMo_results/Empirical_i-sp_network_PC/10_runs",sep=""))
  wbipmod.pc<-read.table("Qvalues_i-spPC_10runs.txt", header =T, sep=",",dec=".")
  wbipmod.pc # weighted modularity values for 10 runs i-sp networks PC site
  mean.quanbimo.cn<-mean(wbipmod.cn$MWB)
  mean.quanbimo.pc<-mean(wbipmod.pc$MWB)
  
# Extract the module partitions obtained with 10 runs with QuanBiMo program for empirical i-sp networks ------------------------------------------

  n<-c(1:10)
  run<-c()
  for(i in 1:length(n)){
  run[i]<-paste("Run_",n[i],sep="")
  }
  run
  quanbimo.files.values.cn<-list()
  quanbimo.files.values.pc<-list()
  quanbimo.cn<-matrix()
  quanbimo.pc<-matrix()
  for(i in 1:length(run)){
      setwd(paste(workdir,"/QuanBiMo_results/Empirical_i-sp_network_CN/10_runs",sep=""))
      quanbimo.files.values.cn[[i]]<-read.table(file=paste("module.composition.i-spCN",run[i],"txt",sep="."), header =T, sep=" ",dec=".")
      quanbimo.files.values.cn[[i]]<-quanbimo.files.values.cn[[i]][order(quanbimo.files.values.cn[[i]]$node),]
      quanbimo.files.values.cn[[i]]<-quanbimo.files.values.cn[[i]][order(quanbimo.files.values.cn[[i]]$type,decreasing=T),] # order first pollinators and after plants
      quanbimo.files.values.cn[[i]]<-quanbimo.files.values.cn[[i]]$module
      setwd(paste(workdir,"/QuanBiMo_results/Empirical_i-sp_network_PC/10_runs",sep=""))
      quanbimo.files.values.pc[[i]]<-read.table(file=paste("module.composition.i-spPC",run[i],"txt",sep="."), header =T, sep=" ",dec=".")
      quanbimo.files.values.pc[[i]]<-quanbimo.files.values.pc[[i]][order(quanbimo.files.values.pc[[i]]$node),]
      quanbimo.files.values.pc[[i]]<-quanbimo.files.values.pc[[i]][order(quanbimo.files.values.pc[[i]]$type,decreasing=T),] # order first pollinators and after plants
      quanbimo.files.values.pc[[i]]<-quanbimo.files.values.pc[[i]]$module
  }
  quanbimo.cn<-do.call(cbind,quanbimo.files.values.cn)
  quanbimo.pc<-do.call(cbind,quanbimo.files.values.pc)
  colnames(quanbimo.cn)<-c(paste(rep("QuanBiMo",10),run,sep="_"))
  quanbimo.cn<-data.frame(nodes=c(1:245),quanbimo.cn)
  colnames(quanbimo.pc)<-c(paste(rep("QuanBiMo",10),run,sep="_"))
  quanbimo.pc<-data.frame(nodes=c(1:186),quanbimo.pc)
  quanbimo.cn # Dataframe with modules returned by 10 runs of QuanBiMo for i-sp network in CN site
  quanbimo.pc # Dataframe with modules returned by 10 runs of QuanBiMo for i-sp network in PC site
  

# COMPARISON OF MODULARITY RESULTS AMONG DIFFERENT METRICS AND AMONG RUNS OF SAME METRIC  ############################################################
######################################################################################################################################################

# Calculate mutual information (as in Thébault 2013) to compare module identification among methods and runs within method ---------------------------

  mod.runs.netcarto.cn # Dataframe with module partitions obtained with 10 runs Netcarto for i-sp network PC site
  mod.runs.netcarto.pc # Dataframe with module partitions obtained with 10 runs Netcarto for i-sp network PC site
  
  mod.bipmod.cn # Dataframe of module partitions obtained with 10 runs bipmod for CN site
  mod.bipmod.pc # Dataframe of module partitions obtained with 10 runs bipmod for PC site

  quanbimo.cn # Table with partitions returned by 10 runs with QuanBiMo of i-sp network in CN site
  quanbimo.pc # Table with partitions returned by 10 runs with QuanBiMo of i-sp network in PC site

  mod.methods<-cbind(mod.runs.netcarto.cn,mod.bipmod.cn[,-1],quanbimo.cn[,-1]) # merge module identifications from different methods

  comparisons<-combn(names(mod.methods)[-1],2)
  mod.methods<-mod.methods[,-1]
  mutual.info<-c()
  for(q in 1:ncol(comparisons)){
      Q<-as.matrix(mod.methods[which(colnames(mod.methods)==comparisons[,q][1])]) # vector giving the group affiliation (as a number) of each node with method 1
      A<-as.matrix(mod.methods[which(colnames(mod.methods)==comparisons[,q][2])]) # vector giving the group affiliation (as a number) of each node with method 2
      # in A and Q, we need to have species ordered in the same way to be able to match both partitions
      S<-length(Q)
      index<-1:S
      H1<-0
      H2<-0
      H12<-0
      for (i in 1:max(Q)) {
        bidon<-Q==i
        Qg<-length(Q[bidon])
        if (Qg>0)  H1<- H1 + Qg*log(Qg/S)
      }
      for (i in 1:max(A)) {
        bidon<-A==i
        Ag<-length(A[bidon])
        if (Ag>0)  H2<- H2 + Ag*log(Ag/S)
      }
      for (i in 1:max(Q)) {
        bidon<-Q==i
        Qg<-length(Q[bidon])
        idQg<-index[bidon]
        if (Qg>0) {
          for (j in 1:max(A)) {
            bidon<-A==j
            Ag<-length(A[bidon])
            idAg<-index[bidon]
            truc<-match(idQg,idAg)
            truc<-length(truc[!is.na(truc)])
            if (truc>0) H12<-H12 + truc*log(truc*S/(Qg*Ag))
          }
        }
      }
      mutinfo<- -2*H12/(H1+H2)
      mutual.info[q]<-round(mutinfo,2)
    }
  mutual.info

  c<-c()
  for(x in 1:length(mutual.info)){
  c[x]<-paste(comparisons[1,x],comparisons[2,x],sep="_vs_") # put a name to all pairwise comparisons
  }
  names(mutual.info)<-c
  mutual.info # mutual information index for pairwise comparisons

  within.netcarto.comp<-mutual.info[grep("NetCarto_",names(mutual.info))]
  within.netcarto.comp<-within.netcarto.comp[-grep("Bipmod_",names(within.netcarto.comp))]
  within.netcarto.comp<-within.netcarto.comp[-grep("QuanBiMo_",names(within.netcarto.comp))]
  within.netcarto.comp # agreement between partitions of netcarto in different runs
  mean(within.netcarto.comp);sd(within.netcarto.comp)

  within.bipmod.comp<-mutual.info[grep("Bipmod_",names(mutual.info))]
  within.bipmod.comp<-within.bipmod.comp[-grep("NetCarto_",names(within.bipmod.comp))]
  within.bipmod.comp<-within.bipmod.comp[-grep("QuanBiMo_",names(within.bipmod.comp))]
  within.bipmod.comp # agreement between partitions of bipmod in different runs
  mean(within.bipmod.comp); sd(within.bipmod.comp)
  
  within.quanbimo.comp<-mutual.info[grep("QuanBiMo_",names(mutual.info))]
  within.quanbimo.comp<-within.quanbimo.comp[-grep("NetCarto_",names(within.quanbimo.comp))]
  within.quanbimo.comp<-within.quanbimo.comp[-grep("Bipmod_",names(within.quanbimo.comp))]
  within.quanbimo.comp # agreement between partitions of quanbimo in different runs
  mean(within.quanbimo.comp); sd(within.quanbimo.comp)

  between.methods.comp1<-mutual.info[grep("Bipmod_",names(mutual.info))]
  between.methods.comp1<-between.methods.comp1[grep("NetCarto_",names(between.methods.comp1))]
  between.methods.comp1 # agreement between partitions of bipmod and netcarto runs
  mean(between.methods.comp1);sd(between.methods.comp1)

  between.methods.comp2<-mutual.info[grep("Bipmod_",names(mutual.info))]
  between.methods.comp2<-between.methods.comp2[grep("QuanBiMo_",names(between.methods.comp2))]
  between.methods.comp2 # agreement between partitions of bipmod and quanbimo runs
  mean(between.methods.comp2);sd(between.methods.comp2)

  between.methods.comp3<-mutual.info[grep("NetCarto_",names(mutual.info))]
  between.methods.comp3<-between.methods.comp3[grep("QuanBiMo_",names(between.methods.comp3))]
  between.methods.comp3 # agreement between partitions of quanbimo and netcarto runs
  mean(between.methods.comp3);sd(between.methods.comp3)

# Plot to compare modularity in empirical i-sp networks with modularity in null i-sp networks with 3 measures -----------------------------------
# Figure 1 in the paper

  unimod.null.cn # unipartite modularity values for 100 null i-sp networks CN site
  unimod.null.pc # unipartite modularity values for 100 null i-sp networks PC site

  bipmod.null.cn # bipartite modularity values for 100 null i-sp networks CN site
  bipmod.null.pc # bipartite modularity values for 100 null i-sp networks PC site

  wbipmod.null.cn<-wbipmod.null.cn[,2]
  wbipmod.null.pc<-wbipmod.null.pc[,2]
  wbipmod.null.cn # weighted bipartite modularity values for 100 null i-sp networks CN site
  wbipmod.null.pc # weighted bipartite modularity values for 100 null i-sp networks PC site

  unimod.emp.cn.i<-sapply(unimod.emp.cn.i,mean) # unipartite modularity for 10 runs i-sp networks CN site
  unimod.emp.pc.i<-sapply(unimod.emp.pc.i,mean) # unipartite modularity for 10 runs i-sp networks PC site

  mean(bipmod.values.cn) # mean bipartite modularity of 10 runs for i-sp networks CN site
  mean(bipmod.values.pc) # mean bipartite modularity of 10 runs for i-sp networks PC site

  mean.quanbimo.cn # mean weighted bipartite modularity of 10 runs for i-sp networks CN site
  mean.quanbimo.pc # mean weighted bipartite modularity of 10 runs for i-sp networks PC site

  results.nulls<-data.frame(site=c(rep("CN",100),rep("PC",100)),measure=c(rep("MU",200),rep("MB",200),
  rep("MWB",200)),mod=c(unimod.null.cn,unimod.null.pc,
                bipmod.null.cn,bipmod.null.pc,
                wbipmod.null.cn,wbipmod.null.pc))
  results.nulls # Dataframe containing values of 100 null i-sp networks for each site with the three different modularity measures
  results.nulls$measure<-relevel(results.nulls$measure,ref="MU")
  op <- par(mfrow=c(1,2),oma=c(3,1,0,0)) # Divide the plot into 2, one plot for each site
  op[[1]]<-boxplot(mod~measure,data=subset(results.nulls,site="CN"),col=c("red","blue","green"),ylab="Modularity",ylim=c(0,0.5)) # Do a boxplot showing results of different measures
  points(y=mean(unimod.emp.cn.i),x=1,pch=8,col="black") # mean value M unimod
  points(y=mean(bipmod.values.cn),x=2,pch=8,col="black") # mean value M bipmod
  points(y=mean.quanbimo.cn,x=3,pch=8,col="black") # mean value M quanbimo
  op[[2]]<-boxplot(mod~measure,data=subset(results.nulls,site="PC"),col=c("red","blue","green"),ylab="Modularity",ylim=c(0,0.5)) # Do a boxplot showing results of different measures
  points(y=mean(unimod.emp.pc.i),x=1,pch=8,col="black") # mean value M unimod
  points(y=mean(bipmod.values.pc),x=2,pch=8,col="black") # mean value M bipmod
  points(y=mean.quanbimo.pc,x=3,pch=8,col="black") # mean value M quanbimo

