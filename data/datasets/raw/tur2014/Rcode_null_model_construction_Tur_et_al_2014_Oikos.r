#########################################################################################################################
## R code for analysis included in:
## Tur C, Olesen JM & Traveset A. (2014)
## Increasing modularity when downscaling networks from species to individuals
## Oikos
#########################################################################################################################


# ---------------------------------------------------------------------------------------------------------------------- #
# Construct a null model to compare network parameters controlling for network size and individual specialization.       #
#----------------------------------------------------------------------------------------------------------------------- #

# Features of null i-sp networks:
# (a) Same size and species composition than empirical i-sp networks.
# (b) Individuals act as generalists sampling from pollen types used by the corresponding species.

# Load data ------------------------------------------------------------------------------------------------------------------
# Data files used here can be found in the following DRYAD data package: http://datadryad.org/resource/doi:10.5061/dryad.63fp5

    networks_i_sp<-list()
    networks_i_sp[[1]]<-read.table(file="weightwebCN_i-sp.txt",sep=",",header=T,na.strings="") # Pollen-transport matrix CN site
    networks_i_sp[[2]]<-read.table(file="weightwebPC_i-sp.txt",sep=",",header=T,na.strings="") # Pollen-transport matrix PC site
    no.samples<-read.table(file="insect.pollinators.txt", sep=",", header=T) # Table with data of sampled species
    samples<-list(subset(no.samples,Site=="CN"),subset(no.samples,Site=="PC")) # Subset data of each site

    library(bipartite)
    
# Construct 100 null i-sp networks --------------------------------------------------------------------------------------------
# Null i-sp networks result from the combination of null i-sp submatrices for each species generated with Patefield algorithm.
# Note that for species with only one individual sampled null i-sp submatrices can't be created.

        spp1<-list()
        for (i in 1:length(samples)){
            spp1[[i]]<-as.character(subset(samples[[i]],samples[[i]]$no.ind==1)$Species)
        }
        spp1 # List of species in each site with only one individual sampled.
        m1<-list()
        for(n in 1:length(networks_i_sp)){
            subnetworks<-list()
            for (i in 1:length(spp1[[n]])){
                  subnetworks[[i]]<-networks_i_sp[[n]][grep(spp1[[n]][i],rownames(networks_i_sp[[n]])),]
                  m1[[n]]<-do.call(rbind,subnetworks)
            }
        }
        m1 # Matrices with species with only one individual sampled.
        spp2<-list()
        for (i in 1:length(samples)){
            spp2[[i]]<-as.character(subset(samples[[i]],samples[[i]]$no.ind>1)$Species)
        }
        spp2 # List of species in each site with > 1 individual
        subnetworks<-list()
        for(n in 1:length(networks_i_sp)){
            m<-list()
            for (i in 1:length(spp2[[n]])){
                m[[i]]<-networks_i_sp[[n]][grep(spp2[[n]][i],rownames(networks_i_sp[[n]])),]
            }
            subnetworks[[n]]<-m
        }
        subnetworks # Submatrices i-sp for all species with > individual sampled.
        NULL.networks<-list()
        for(n in 1:length(subnetworks)){
                null.subnetworks<-list()
                # Generate 1,000 null i-sp submatrices for each species:
                for (i in 1:length(subnetworks[[n]])){
                      null.subnetworks[[i]]<-nullmodel(subnetworks[[n]][[i]],N=100,method=1) # method 1 = Fixed row and column totals.
                      for(j in 1:100){
                          colnames(null.subnetworks[[i]][[j]])<-colnames(subnetworks[[n]][[i]])
                          rownames(null.subnetworks[[i]][[j]])<-rownames(subnetworks[[n]][[i]])
                      }
                }
          null.networks<-list()
                # Combine null i-sp submatrices to create the complete null i-sp networks
                for(j in 1:100){
                    m2<-list()
                    null<-matrix()
                    for (i in 1:length(null.subnetworks)){
                        m2[[i]]<-null.subnetworks[[i]][[j]]
                        null<-do.call(rbind,m2)
                    }
                    null.networks[[j]]<-rbind(m1[[n]],null)
                    null.networks[[j]]<-null.networks[[j]][order(rownames(null.networks[[j]])),]
                }
          NULL.networks[[n]]<-null.networks
        }
        NULL.networks[[1]] # List of 100 null weighted i-sp networks for CN site
        NULL.networks[[2]] # List of 100 null weighted i-sp networks for PC site

        
# Run this piece of code if we want to save the NULL i-sp matrices to use posteriorly for modularity computation ----------------------------------------------------------

    for (i in 1:length(NULL.networks[[1]])){ # save in format needed for BIPMOD. First we need to convert networks into binary form.
        write.table(replace(NULL.networks[[1]][[i]],NULL.networks[[1]][[i]]>0,1),file=paste("NULL.binCNi-sp.network",i,".txt",sep=""),sep="\t",row.names=F,col.names=F)
    }

    for (i in 1:length(NULL.networks[[2]])){
        write.table(replace(NULL.networks[[2]][[i]],NULL.networks[[2]][[i]]>0,1),file=paste("NULL.binPCi-sp.network",i,".txt",sep=""),sep="\t",row.names=F,col.names=F)
    }

    webname<-c()
    for (i in 1:length(NULL.networks[[1]])){ # save in format needed for NetCarto, i.e. a list of edges.
        webname[i]<-paste("NULL.binCNi-sp.edges",i,sep="")
        web2edges(NULL.networks[[1]][[i]],webName=webname[i],weight.column=F,out.files="edges")
    }
    
    webname<-c()
    for (i in 1:length(NULL.networks[[2]])){
        webname[i]<-paste("NULL.binPCi-sp.edges",i,sep="")
        web2edges(NULL.networks[[2]][[i]],webName=webname[i],weight.column=F,out.files="edges")
    }


#---------------------------------------------------------------------------------------------------------------#
#        Calculate network parameters of null i-sp networks for comparison with the empirical ones              #
#---------------------------------------------------------------------------------------------------------------#

# Total interactions in null i-sp networks -----------------------------------------------------------------------

    null.links<-list()
    for(j in 1:length(NULL.networks)){
        links<-c()
        for (i in 1:length(NULL.networks[[j]])){
            links[i]<-sum(ifelse(NULL.networks[[j]][[i]]>0,1,0))
        }
        null.links[[j]]<-links
    }
    null.links
    lapply(null.links,mean)
    lapply(null.links,sd)
    
# Network connectance --------------------------------------------------------------------------------------------------------

    null.connectance<-list()
    for(j in 1:length(NULL.networks)){
        C<-c()
        for (i in 1:length(NULL.networks[[j]])){
            C[i]<-networklevel(NULL.networks[[j]][[i]],index="connectance")
        }
        null.connectance[[j]]<-C
    }
    null.connectance
    lapply(null.connectance,mean)
    lapply(null.connectance,sd)

# Nestedness: nodf -----------------------------------------------------------------------------------------------------------------------

    null.nodf<-list()
    for(j in 1:length(NULL.networks)){
        NODF<-list()
        nodf<-c()
    for (i in 1:length(NULL.networks[[j]])){
        NODF[[i]]<-nestednodf(NULL.networks[[j]][[i]], weighted=FALSE)
        nodf[i]<-NODF[[i]]$statistic[3]
    }
    null.nodf[[j]]<-nodf
    }
    null.nodf
    lapply(null.nodf,mean)
    lapply(null.nodf,sd)

    # Test NODF significance:
      sig.NODF<-list()
      for (i in 1:length(NULL.networks[[1]])){
          sig.NODF[[i]]<-oecosimu(replace(NULL.networks[[1]][[i]],NULL.networks[[1]][[i]]>0,1),nestednodf,method="r2dtable",nsimul=1000,alternative=c("less"))
      }
      sig.NODF

      sig.NODF<-list()
        for (i in 1:length(NULL.networks[[2]])){
            sig.NODF[[i]]<-oecosimu(replace(NULL.networks[[2]][[i]],NULL.networks[[2]][[i]]>0,1),nestednodf,method="r2dtable",nsimul=1000,alternative=c("less"))
        }
        sig.NODF

# Calculate bipartite weighted modularity of null i-sp networks ---------------------------------------------------------------------------------------------------


    for(j in 1:length(NULL.networks)){ # matrix transformation
    for(i in 1:length(NULL.networks[[j]])){
          NULL.networks[[j]][[i]]<-log10(NULL.networks[[j]][[i]]+1)
          NULL.networks[[j]][[i]]<-NULL.networks[[j]][[i]]*10
          NULL.networks[[j]][[i]]<-round(NULL.networks[[j]][[i]],0)
    }
    }
    NULL.networks

    modules.100nulls.CN <- sapply(NULL.networks[[1]], computeModules)
    mod.nulls.CN <- sapply(modules.100nulls.CN, function(x) x@likelihood)
    mod.nulls.CN
    mean(mod.nulls.CN);sd(mod.nulls.CN)
    MvaluesCN<-data.frame(network=c(paste("null",c(1:100))),modularity=mod.nulls.CN)
    write.table(MvaluesCN,"Qvalues_i-sp_100nulls.CN.txt",row.names=F) # Weighted bipartite modularity values of 100 null i-sp networks CN site

    modules.100nulls.PC <- sapply(NULL.networks[[2]], computeModules)
    mod.nulls.PC <- sapply(modules.100nulls.PC, function(x) x@likelihood)
    mod.nulls.PC
    mean(mod.nulls.PC);sd(mod.nulls.PC)
    MvaluesPC<-data.frame(network=c(paste("null",c(1:100))),modularity=mod.nulls.PC)
    write.table(MvaluesPC,"Qvalues_i-sp_100nulls.PC.txt",row.names=F) # Weighted bipartite modularity values of 100 null i-sp networks PC site
