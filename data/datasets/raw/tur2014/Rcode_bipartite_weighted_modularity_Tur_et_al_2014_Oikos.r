#######################################################################################################################################
## R code for analysis included in:
## Tur C, Olesen JM & Traveset A. (2014)
## Increasing modularity when downscaling networks from species to individuals
## Oikos
#######################################################################################################################################

## Data files for the following analyses can be found in DRYAD data package: http://datadryad.org/resource/doi:10.5061/dryad.63fp5
## Read also: Dormann & Strauss(2014) http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12139/pdf

## WEIGHTED BIPARTITE MODULARITY IN EMPIRICAL SP-SP NETWORKS #################################################################

# Load data and R packages -----------------------------------------------------------------------------------------

    library(bipartite) # version 2.03

    sp.sp_networks<-list()
    sp.sp_networks[[1]]<-read.table(file="weightwebCN_sp-sp.txt",sep=",",header=T, na.strings="")
    sp.sp_networks[[2]]<-read.table(file="weightwebPC_sp-sp.txt",sep=",",header=T, na.strings="")
    sp.sp_networks

    # Prepare matrices:
    for(i in 1:length(sp.sp_networks)){
          sp.sp_networks[[i]]<-log10(sp.sp_networks[[i]]+1) #  Log-transform the matrices
          sp.sp_networks[[i]]<-sp.sp_networks[[i]]*10 # Multiply to eliminate numbers < 0 (necessary for null models)
          sp.sp_networks[[i]]<-round(sp.sp_networks[[i]],0) # round numbers (necessary for null models)
    }
    sp.sp_networks

# Compute modularity for bipartite weighted networks (MBW) --------------------------------------------------------

    mod1 <- computeModules(web=sp.sp_networks[[1]], steps=1E8)
    mod1@likelihood # Bipartite weighted modularity sp-sp CN site

    mod2 <- computeModules(web=sp.sp_networks[[2]], steps=1E8)
    mod2@likelihood # Bipartite weighted modularity sp-sp PC site

# Compute null model expectations and turn the observed value of MBW into a z-score --------------------------------

    nulls <- nullmodel(sp.sp_networks[[1]], N=100, method="r2dtable")
    modules.nulls <- sapply(nulls, computeModules)
    like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
    (z <- (mod1@likelihood - mean(like.nulls))/sd(like.nulls))
    like.nulls # M values of randomizations CN site

    nulls2 <- nullmodel(sp.sp_networks[[2]], N=100, method="r2dtable")
    modules.nulls2 <- sapply(nulls2, computeModules)
    like.nulls2 <- sapply(modules.nulls2, function(x) x@likelihood)
    (z <- (mod2@likelihood - mean(like.nulls2))/sd(like.nulls2))
    like.nulls2 # M values of randomizations PC site

# Save relevant results ------------------------------------------------------------------------------------------

    QspCN<-mod1@likelihood # MWB empirical sp-sp network CN site
    QrspCN<-like.nulls # MWB randomizations
    QvaluesCN<-data.frame(network=c("empirical",paste("random",c(1:100))),Q=c(QspCN,QrspCN))
    write.table(QvaluesCN,"Qvalues_sp-spCN.txt",row.names=F)

    cz_spCN_plant<-czvalues(mod1,level="higher") # c-z values for plant species
    cz.table<-data.frame(c=cz_spCN_plant$c,z=cz_spCN_plant$z)# put results in a table
    cz_spCN_pol<-czvalues(mod1,level="lower") # c-z values for pollinator species
    cz.table2<-data.frame(c=cz_spCN_pol$c,z=cz_spCN_pol$z)
    cz.table<-rbind(cz.table,cz.table2)
    write.table(cz.table,"cz.table.sp-spCN.txt")

    modulesCNsp<- listModuleInformation(mod1) # modules composition
    modCN<-list() # Create and save a table with the oomposition of modules
    for(i in 1:length(listModuleInformation(mod1)[[2]])){
        modCN[[i]]<-rbind(
                        cbind(listModuleInformation(mod1)[[2]][[i]][[1]],
                                rep("pollinator",length(listModuleInformation(mod1)[[2]][[i]][[1]])),
                                rep(i,length(listModuleInformation(mod1)[[2]][[i]][[1]]))),
                        cbind(listModuleInformation(mod1)[[2]][[i]][[2]],
                                rep("plant",length(listModuleInformation(mod1)[[2]][[i]][[2]])),
                                rep(i,length(listModuleInformation(mod1)[[2]][[i]][[2]])))
                        )
      }
    module.composition.CN<-as.data.frame(do.call(rbind,modCN))
    names(module.composition.CN)<-c("node","type","module")
    module.composition.CN
    write.table(module.composition.CN,"module.composition.sp-spCN.txt",row.names=F,quote=F)

    QspPC<-mod2@likelihood # MWB empirical sp-sp network PC site
    QrspPC<-like.nulls2 # MWB randomizations
    QvaluesPC<-data.frame(network=c("empirical",paste("random",c(1:100))),Q=c(QspPC,QrspPC))
    write.table(QvaluesPC,"Qvalues_sp-spPC.txt",row.names=F)

    cz_spPC_plant<-czvalues(mod2,level="higher") # c-z values for plant species
    cz.table3<-data.frame(c=cz_spPC_plant$c,z=cz_spPC_plant$z)# put results in a table
    cz_spPC_pol<-czvalues(mod2,level="lower") # c-z values for pollinator species
    cz.table4<-data.frame(c=cz_spPC_pol$c,z=cz_spPC_pol$z)
    cz.tablePC<-rbind(cz.table3,cz.table4)
    write.table(cz.tablePC,"cz.table.sp-spPC.txt")

    modulesPCsp<-listModuleInformation(mod2)
    modPC<-list() # Create and save a table with the oomposition of modules
    for(i in 1:length(listModuleInformation(mod2)[[2]])){
        modPC[[i]]<-rbind(
                        cbind(listModuleInformation(mod2)[[2]][[i]][[1]],
                                rep("pollinator",length(listModuleInformation(mod2)[[2]][[i]][[1]])),
                                rep(i,length(listModuleInformation(mod2)[[2]][[i]][[1]]))),
                        cbind(listModuleInformation(mod2)[[2]][[i]][[2]],
                                rep("plant",length(listModuleInformation(mod2)[[2]][[i]][[2]])),
                                rep(i,length(listModuleInformation(mod2)[[2]][[i]][[2]])))
                        )
      }
    module.composition.PC<-as.data.frame(do.call(rbind,modPC))
    names(module.composition.PC)<-c("node","type","module")
    module.composition.PC
    write.table(module.composition.PC,"module.composition.sp-spPC.txt",row.names=F,quote=F)


## WEIGHTED BIPARTITE MODULARITY IN EMPIRICAL I-SP NETWORKS #################################################################

# Load data ----------------------------------------------------------------------------------------------------------------

    i.sp_networks<-list()
    i.sp_networks[[1]]<-read.table(file="weightwebCN_i-sp.txt",sep=",",header=T, na.strings="")
    i.sp_networks[[2]]<-read.table(file="weightwebPC_i-sp.txt",sep=",",header=T, na.strings="")
    i.sp_networks

    for(i in 1:length(i.sp_networks)){
          i.sp_networks[[i]]<-log10(i.sp_networks[[i]]+1) # Log-transform the matrices
          i.sp_networks[[i]]<-i.sp_networks[[i]]*10 # Multiply to eliminate numbers < 0 (necessary for null models)
          i.sp_networks[[i]]<-round(i.sp_networks[[i]],0) # Round numbers (necessary for null models)
    }
    i.sp_networks

# Compute modularity for bipartite weighted networks (MBW) ---------------------------------------------------------

    mod3 <- computeModules(web=i.sp_networks[[1]], steps=1E8)
    mod3@likelihood # Bipartite weighted modularity i-sp CN site

    mod4 <- computeModules(web=i.sp_networks[[2]], steps=1E8)
    mod4@likelihood # Bipartite weighted modularity i-sp PC site

# Compute null model expectations and turn the observed value of MBW into a z-score --------------------------------

    nulls3 <- nullmodel(i.sp_networks[[1]], N=100, method="r2dtable")
    modules.nulls3 <- sapply(nulls3, computeModules)
    like.nulls3 <- sapply(modules.nulls3, function(x) x@likelihood)
    (z <- (mod3@likelihood - mean(like.nulls3))/sd(like.nulls3)) # Z-score
    like.nulls3 # MWB values of randomizations CN site

    nulls4 <- nullmodel(i.sp_networks[[2]], N=100, method="r2dtable")
    modules.nulls4 <- sapply(nulls4, computeModules)
    like.nulls4 <- sapply(modules.nulls4, function(x) x@likelihood)
    (z <- (mod4@likelihood - mean(like.nulls4))/sd(like.nulls4))
    like.nulls4 # MWB values of randomizations PC site

# Save relevant results ---------------------------------------------------------------------------------------------

    QiCN<-mod3@likelihood # MWB empirical i-sp network CN site
    QriCN<-like.nulls3 # MWB randomizations
    QvaluesCNi<-data.frame(network=c("empirical",paste("random",c(1:100))),Q=c(QiCN,QriCN))
    write.table(QvaluesCNi,"Qvalues_i-spCN.txt",row.names=F)

    cz_iCN_plant<-czvalues(mod3,level="higher") # c-z values for plant species
    cz.table<-data.frame(c=cz_iCN_plant$c,z=cz_iCN_plant$z)# put results in a table
    cz_iCN_pol<-czvalues(mod3,level="lower") # c-z values for pollinator species
    cz.table2<-data.frame(c=cz_iCN_pol$c,z=cz_iCN_pol$z)
    cz.table<-rbind(cz.table,cz.table2)
    write.table(cz.table,"cz.table.i-spCN.txt")

    modulesCNi<- listModuleInformation(mod3) # modules composition
    modCNi<-list() # Create and save a table with the oomposition of modules
    for(i in 1:length(listModuleInformation(mod3)[[2]])){
        modCNi[[i]]<-rbind(
                        cbind(listModuleInformation(mod3)[[2]][[i]][[1]],
                                rep("pollinator",length(listModuleInformation(mod3)[[2]][[i]][[1]])),
                                rep(i,length(listModuleInformation(mod3)[[2]][[i]][[1]]))),
                        cbind(listModuleInformation(mod3)[[2]][[i]][[2]],
                                rep("plant",length(listModuleInformation(mod3)[[2]][[i]][[2]])),
                                rep(i,length(listModuleInformation(mod3)[[2]][[i]][[2]])))
                        )
      }
    module.composition.CNi<-as.data.frame(do.call(rbind,modCNi))
    names(module.composition.CNi)<-c("node","type","module")
    module.composition.CNi
    write.table(module.composition.CNi,"module.composition.i-spCN.txt",row.names=F,quote=F)


    QiPC<-mod4@likelihood # MWB empirical i-sp network PC site
    QriPC<-like.nulls4 # MWB randomizations
    QvaluesPCi<-data.frame(network=c("empirical",paste("random",c(1:100))),Q=c(QiPC,QriPC))
    write.table(QvaluesPCi,"Qvalues_i-spPC.txt",row.names=F)

    cz_iPC_plant<-czvalues(mod4,level="higher") # c-z values for plant species
    cz.table3<-data.frame(c=cz_iPC_plant$c,z=cz_iPC_plant$z)# put results in a table
    cz_iPC_pol<-czvalues(mod4,level="lower") # c-z values for pollinator species
    cz.table4<-data.frame(c=cz_iPC_pol$c,z=cz_iPC_pol$z)
    cz.tableiPC<-rbind(cz.table3,cz.table4)
    write.table(cz.tableiPC,"cz.table.i-spPC.txt")

    modulesPCi<-listModuleInformation(mod4)
    modPCi<-list() # Create and save a table with the oomposition of modules
    for(i in 1:length(listModuleInformation(mod4)[[2]])){
        modPCi[[i]]<-rbind(
                        cbind(listModuleInformation(mod4)[[2]][[i]][[1]],
                                rep("pollinator",length(listModuleInformation(mod4)[[2]][[i]][[1]])),
                                rep(i,length(listModuleInformation(mod4)[[2]][[i]][[1]]))),
                        cbind(listModuleInformation(mod4)[[2]][[i]][[2]],
                                rep("plant",length(listModuleInformation(mod4)[[2]][[i]][[2]])),
                                rep(i,length(listModuleInformation(mod4)[[2]][[i]][[2]])))
                        )
      }
    module.composition.PCi<-as.data.frame(do.call(rbind,modPCi))
    names(module.composition.PCi)<-c("node","type","module")
    module.composition.PCi
    write.table(module.composition.PCi,"module.composition.i-spPC.txt",row.names=F,quote=F)
