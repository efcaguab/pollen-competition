#########################################################################################################################
## R code for analysis included in:
## Tur C, Olesen JM & Traveset A. (2014)
## Increasing modularity when downscaling networks from species to individuals
## Oikos
#########################################################################################################################

## This script was written by C. Tur, for any comments please mail to: cris.tur.espinosa@gmail.com
## For quantitative data from pollen-transport networks see also: http://datadryad.org/resource/doi:10.5061/dryad.63fp5

## To calculate unipartite modularity use Netcarto program: http://etseq.urv.cat/seeslab/downloads/network-cartography-netcarto/
## Reference: Guimerà R. & Amaral LAN (2005) http://dx.doi.org/10.1088/1742-5468/2005/02/P02001

## To calculate bipartite modularity use BIPMOD program: http://onlinelibrary.wiley.com/doi/10.1111/jbi.12015/suppinfo
## Reference: Thébault (2014) http://onlinelibrary.wiley.com/doi/10.1111/jbi.12015/pdf

## To calculate weighted bipartite modularity use function computeModules in package bipartite.
## See file 'Rcode_bipartite_weighted_modularity.r' included in this data package for calculating weighted bipartite modularity in R.
## Reference: Dormann & Strauss(2014) http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12139/pdf


# Load packages needed for analysis ---------------------------------------------------------------------------------------------------------

    library(vegan) # For community analyses
    library(Deducer)  # For likelihood-test
    library(agricolae) # For non-parametric post-hoc multiple comparisons
    library(bipartite) # For network analysis (this code is working with version 1.17)
    library(seriation) # For plotting dissimilarity matrices
    library(mlogit) # For estimation of multinomial logit models
    library(nnet) # For multinomial logit models
    library(ggplot2) # For plots
    library(reshape2) # For reshaping data
    library(effects) # For model effects displays
    
# Load files and prepare input data -------------------------------------------------------------------------------------------------------

    sp.sp_networks<-list()
    sp.sp_networks[[1]]<-read.table(file="binwebCN_sp-sp.txt",sep=",",header=T, na.strings="")
    sp.sp_networks[[2]]<-read.table(file="binwebPC_sp-sp.txt",sep=",",header=T, na.strings="")
    sp.sp_networks # List of interaction matrices at species-species level

    i.sp_networks<-list()
    i.sp_networks[[1]]<-read.table(file="binwebCN_i-sp.txt",sep=",",header=T, na.strings="")
    i.sp_networks[[2]]<-read.table(file="binwebPC_i-sp.txt",sep=",",header=T, na.strings="")
    i.sp_networks # List of interaction matrices at individuals-species level

    networks<-list()
    networks[[1]]<-sp.sp_networks[[1]] # Sp-sp network CN site
    networks[[2]]<-i.sp_networks[[1]] # Empirical i-sp network CN site
    networks[[3]]<-sp.sp_networks[[2]] # Sp-sp network PC site
    networks[[4]]<-i.sp_networks[[2]] # Empirical i-sp network PC site
    networks # List of ALL empirical networks (species-species and individuals-species)

    data<-read.table(file="ind_prop_list.txt",sep=",",header=T, na.strings="")
    ind_list<-list()
    ind_list[[1]]<-subset(data,data$Site=="CN")
    ind_list[[2]]<-subset(data,data$Site=="PC")
    for(i in 1:length(ind_list)){
        ind_list[[i]]$module<-as.factor(ind_list[[i]]$module)
        ind_list[[i]]$Site<-factor(ind_list[[i]]$Site)
        ind_list[[i]]$phenol<-factor(ind_list[[i]]$phenol,levels=c("may","june","july","august"))
        ind_list[[i]]$role<-factor(ind_list[[i]]$role,levels=c("P","C","MH","NH"))
        ind_list[[i]]$label<-as.character(ind_list[[i]]$label)
        ind_list[[i]]$species<-as.character(ind_list[[i]]$species)
        ind_list[[i]]$c<-as.numeric(as.character(ind_list[[i]]$c))
        ind_list[[i]]$z<-as.numeric(as.character(ind_list[[i]]$z))
        ind_list[[i]]$code<-as.character(ind_list[[i]]$code)
        ind_list[[i]]$sex<-factor(ind_list[[i]]$sex,levels=c("f","m"))
        }
    ind_list # List of all insect individuals and properties (i.e. module, phenology, topological role...)

    data<-read.table(file="plant_prop_list.txt",sep=",",header=T, na.strings="")
    plant_list<-list()
    plant_list[[1]]<-subset(data,data$site=="CN")
    plant_list[[2]]<-subset(data,data$site=="PC")
    for(i in 1:length(plant_list)){
        plant_list[[i]]$module<-as.factor(plant_list[[i]]$module)
        plant_list[[i]]$site<-factor(plant_list[[i]]$site)
        plant_list[[i]]$pheno_peak<-factor(plant_list[[i]]$pheno_peak,levels=c("may","june","july","august"))
        plant_list[[i]]$role<-factor(plant_list[[i]]$role,levels=c("P","C","MH","NH"))
        plant_list[[i]]$c<-as.numeric(as.character(plant_list[[i]]$c))
        plant_list[[i]]$z<-as.numeric(as.character(plant_list[[i]]$z))
        plant_list[[i]]$label<-as.character(plant_list[[i]]$label)
    }
    plant_list # List of all plant pollen types and properties

    roles<-list()
    for(i in 1:length(ind_list)){
        roles[[i]]<-rbind(ind_list[[i]][,c(2,3,11,12,13,10)],plant_list[[i]][,c(2,3,6,7,8,9)])
        roles[[i]]<-roles[[i]][complete.cases(roles[[i]]),]
        roles[[i]]$module<-factor(roles[[i]]$module)
        roles[[i]]$role<-factor(roles[[i]]$role,levels=c("P","C","MH","NH"))
    }
    roles # Table with topological roles of nodes in empirical i-sp networks based on within- and among module connectivities

    ind.specialization<-read.table(file="WIC.TNW_values.txt",sep=",",header=T, na.strings="")
    ind.specialization # Table with individual specialization index (WIC/TNW) for 21 selected species
                       # for details on index calculation see script in: http://datadryad.org/resource/doi:10.5061/dryad.63fp5

    CNmods<-list()
    CNmods[[1]]<-read.table(file="CNmod1.txt",sep=",",header=T,na.strings="")
    CNmods[[2]]<-read.table(file="CNmod2.txt",sep=",",header=T,na.strings="")
    CNmods[[3]]<-read.table(file="CNmod3.txt",sep=",",header=T,na.strings="")
    CNmods[[4]]<-read.table(file="CNmod4.txt",sep=",",header=T,na.strings="")
    CNmods[[5]]<-read.table(file="CNmod5.txt",sep=",",header=T,na.strings="")
    CNmods[[6]]<-read.table(file="CNmod6.txt",sep=",",header=T,na.strings="")
    CNmods # List of interaction matrices for each module in empirical i-sp network CN site (partitions obtained with unipartite modularity)

    PCmods<-list()
    PCmods[[1]]<-read.table(file="PCmod1.txt",sep=",",header=T,na.strings="")
    PCmods[[2]]<-read.table(file="PCmod2.txt",sep=",",header=T,na.strings="")
    PCmods[[3]]<-read.table(file="PCmod3.txt",sep=",",header=T,na.strings="")
    PCmods[[4]]<-read.table(file="PCmod4.txt",sep=",",header=T,na.strings="")
    PCmods[[5]]<-read.table(file="PCmod5.txt",sep=",",header=T,na.strings="")
    PCmods # List of interaction matrices for each module in empirical i-sp network PC site (partitions obtained with unipartite modularity)


# (1) ANALYSIS OF NETWORK STRUCTURE ######################################################################################################

# Number of nodes in networks (A = insect pollinators = rows, P = pollen types = columns, A + P = total) ------------------------------

    nodes<-list()
    for (i in 1:length(networks)){
        nodes[[i]]<-dim(networks[[i]])
        nodes[[i]][3]<-sum(dim(networks[[i]]))
    }
    nodes

# Number of nodes in unipartite modules ------------------------------------------------------------------------------------------------

    CNmods_nodes<-list()
    for (i in 1:length(CNmods)){
        CNmods_nodes[[i]]<-dim(CNmods[[i]])
        CNmods_nodes[[i]][3]<-sum(dim(CNmods[[i]]))
    }
    CNmods_nodes

    PCmods_nodes<-list()
    for (i in 1:length(PCmods)){
        PCmods_nodes[[i]]<-dim(PCmods[[i]])
        PCmods_nodes[[i]][3]<-sum(dim(PCmods[[i]]))
    }
    PCmods_nodes

# Network size -----------------------------------------------------------------------------------------------------

    size<-list()
    for (i in 1:length(networks)){
        size[[i]]<-nodes[[i]][1]*nodes[[i]][2]
    }
    size

# Average links per node --------------------------------------------------------------------------------------------

    LD<-list()
    for (i in 1:length(networks)){
        LD[[i]]<-networklevel(networks[[i]],index="links per species")
    }
    LD

# Total number of interactions ---------------------------------------------------------------------------------------

    I<-c()
    for (i in 1:length(networks)){
        I[i]<-nodes[[i]][3]*LD[[i]]
    }
    I

# Network connectance ------------------------------------------------------------------------------------------------

    C<-c()
    for (i in 1:length(networks)){
        C[i]<-networklevel(networks[[i]],index="connectance")
    }
    C

# Connectance inside modules -----------------------------------------------------------------------------------------

    C_CNmods<-list()
    for(i in 1:length(CNmods)){
        C_CNmods[[i]]<-networklevel(CNmods[[i]],index="connectance")
    }
    C_CNmods

    C_PCmods<-list()
    for(i in 1:length(PCmods)){
        C_PCmods[[i]]<-networklevel(PCmods[[i]],index="connectance")
    }
    C_PCmods

# Nestedness NODF in empirical networks -------------------------------------------------------------------------------------------------

    NODF<-list()
    for (i in 1:length(networks)){
        NODF[[i]]<-nestednodf(networks[[i]], weighted=FALSE)
    }
    NODF
    # NODF significance (check whether empirical NODF is higher than expected by random):
    sig.NODF<-list()
    for (i in 1:length(networks)){
        sig.NODF[[i]]<-oecosimu(replace(networks[[i]],networks[[i]]>0,1),nestednodf,method="r2dtable",nsimul=1000,alternative=c("less"))
    }
    sig.NODF

# Nestedness NODF inside each empirical module --------------------------------------------------------------------------------------------

    nodf<-list()
    for(i in 1:length(CNmods)){
        nodf[[i]]<-oecosimu(CNmods[[i]],nestednodf,method="r2dtable",nsimul=1000,alternative=c("less"))
    }
    nodf

    nodf<-list()
    for(i in 1:length(PCmods)){
        nodf[[i]]<-oecosimu(PCmods[[i]],nestednodf,method="r2dtable",nsimul=1000,alternative=c("less"))
    }
    nodf

# Explore the distribution of species and individuals in modules of empirical i-sp networks ----------------------------------------------

    table_i.mod<-list()
    for(i in 1:length(ind_list)){
        table_i.mod[[i]]<-table(ind_list[[i]]$sp,ind_list[[i]]$module)
    }
    table_i.mod # Table with number of individuals in each module

    no_ind<-list()
    for(i in 1:length(ind_list)){
        no_ind[[i]]<-margin.table(table_i.mod[[i]],1)
    }
    no_ind # Number of individuals sampled for each species

    tot_mod<-list()
    table1<-list()
    table_sp.mod<-list()
    for(i in 1:length(ind_list)){
        table1[[i]]<-cbind(table_i.mod[[i]],no_ind[[i]])
        tot_mod[[i]]<-margin.table(table1[[i]],2)
        table_sp.mod[[i]]<-rbind(table1[[i]],tot_mod[[i]])
    }
    table_sp.mod # Table with number of individuals from each species in each module (with row and column totals)

# Number of insect species per module in empirical i-sp networks --------------------------------------------------------------------------------

    no_sp<-list()
    for(i in 1:length(ind_list)){
        no.sp<-c()
        for(n in 1:ncol(table_i.mod[[i]])){
            no.sp[n]<-length(subset(table_i.mod[[i]][,n],table_i.mod[[i]][,n]>0))
        }
        no_sp[[i]]<-no.sp
    }
    no_sp # Number of different insect spp in each module

# Phenological composition of modules in empirical i-sp networks ----------------------------------------------------------------------------

    table_phe.mod<-list()
    for(i in 1:length(ind_list)){
        table_phe.mod[[i]]<-table(ind_list[[i]]$phenol,ind_list[[i]]$module)
    }
    table_phe.mod  # Phenology of insect individuals within each module

    table_phe.mod.plant<-list()
    for(i in 1:length(plant_list)){
        table_phe.mod.plant[[i]]<-table(plant_list[[i]]$pheno_peak,plant_list[[i]]$module)
    }
    table_phe.mod.plant # Phenology of plant pollen types within each module

    phe.tot<-list()
    for(i in 1:length(plant_list)){
        phe.tot[[i]]<-table_phe.mod[[i]]+table_phe.mod.plant[[i]]
    }
    phe.tot # Total phenological composition of modules

    # Pie charts phenological composition of modules:
    for(n in 1:length(phe.tot)){
        for(i in 1:ncol(phe.tot[[n]])){
            dev.new()
            pie(phe.tot[[n]][,i],labels=NA,col=c("red2","blue","green3","yellow"),main=paste("Module",colnames(phe.tot[[n]])[i]))
        }
    }

# Topological roles in empirical i-sp networks ---------------------------------------------------------------------------------------------

    r<-list()
    for(i in 1:length(roles)){
    r[[i]]<-table(roles[[i]]$role)
    }
    r # Number of connectors, peripherals, network hubs or module hubs in null models

    mod.role<-list()
    for(i in 1:length(roles)){
        mod.role[[i]]<-table(roles[[i]]$role,roles[[i]]$module)
    }
    mod.role # Number of connectors, peripherals, network hubs or module hubs within each module

# Draw zc-plot (as in Olesen et al. 2007) ---------------------------------------------------------------------------------------

    for(i in 1:length(roles)){
        dev.new()
        plot(roles[[i]]$c,roles[[i]]$z,col=roles[[i]]$role,xlim=c(0,1),ylim=c(-1,6),pch=21, bg=roles[[i]]$role,
        xlab="Among-module connectivity, c",ylab="Within-module degree, z",cex=1.2)
        segments(0.62,-1.5,0.62,6.5)
        segments(-0.5,2.5,1.5,2.5)
        text(0.9,6,labels="Network hubs",col="blue")
        text(0.1,6,labels="Module hubs",col="green3")
        text(0.1,-1,labels="Peripherals",col="black")
        text(0.9,-1,labels="Connectors",col="red")
    }


# (2) DISTRIBUTION OF CONSPECIFIC INDIVIDUALS AMONG MODULES ###################################################################################

# In how many modules can we find conspecific individuals of selected species (>= 5 individuals captured)? ------------------------------------

    nmods.sp<-c()
    for(i in 1:nrow(table_i.mod[[1]])){
        nmods.sp[i]<-length(subset(table_i.mod[[1]][i,],table_i.mod[[1]][i,]>0))
    }
    names(nmods.sp)<-rownames(table_i.mod[[1]])
    subset(cbind(nmods.sp,no_ind[[1]])[,1],cbind(nmods.sp,no_ind[[1]])[,2]>=5) # Number of modules a species is member

    nmods.sp2<-c()
    for(i in 1:nrow(table_i.mod[[2]])){
        nmods.sp2[i]<-length(subset(table_i.mod[[2]][i,],table_i.mod[[2]][i,]>0))
    }
    names(nmods.sp2)<-rownames(table_i.mod[[2]])
    subset(cbind(nmods.sp2,no_ind[[2]])[,1],cbind(nmods.sp2,no_ind[[2]])[,2]>=5)

    mean(c(subset(cbind(nmods.sp,no_ind[[1]])[,1],cbind(nmods.sp,no_ind[[1]])[,2]>=5),subset(cbind(nmods.sp2,no_ind[[2]])[,1],cbind(nmods.sp2,no_ind[[2]])[,2]>=5)))

# Measure quantitatively the spreading of conspecific individuals among different modules ------------------------------------------------------

    iqv<-list()
    for(n in 1:length(table_i.mod)){
        table_pi2<-(table_i.mod[[n]]/rowSums(table_i.mod[[n]]))^2
        table_pi2<-as.matrix(table_pi2)
        IQV<-matrix(nrow=nrow(table_i.mod[[n]]))
        rownames(IQV)<-rownames(table_i.mod[[n]])
        for(i in 1:nrow(table_i.mod[[n]])){
            IQV[i,]<-(ncol(table_i.mod[[n]])/(ncol(table_i.mod[[n]])-1))*(1-sum(table_pi2[i,]))
        }
        iqv[[n]]<-IQV
        iqv[[n]]<-cbind(round(iqv[[n]],3),no_ind[[n]])
        iqv[[n]]<-subset(iqv[[n]][,1],iqv[[n]][,2]>=5)
    }
    iqv # Measure of module heterogeneity membership of species
    mean(iqv[[1]])
    mean(iqv[[2]])
    
# Is species heterogeneity in module membership related with the degree of individual specialization (WIC/TNW)? -----------------------------

    disp<-data.frame(species=ind.specialization$Species,IQV=c(iqv[[1]],iqv[[2]]),WIC.TNW=ind.specialization$WIC.TNW)
    disp
    summary(lm(disp$IQV~disp$WIC.TNW))

    cor.test(ind.specialization$no.ind,disp$IQV,method="spearman")
    cor.test(ind.specialization$pheno,disp$IQV,method="spearman")
    

# (3) BIOLOGICAL FACTORS AND MODULARITY ############################################################################################################

# Pollen resource niche partitioning among individuals as driver of modularity --------------------------------------------------------------------

    ind.CN<-ind_list[[1]][-25,] # remove individual from isolated module 7
    i.sp.CN<-i.sp_networks[[1]][-25,] # remove individual from isolated module 7 from the interaction matrix
    disCN<-vegdist(i.sp.CN,method="bray",binary=TRUE) # Obtain matrix of distances among pairs of individuals
    mrp<-mrpp(disCN,ind.CN$mod,permutations=1000) # Multi-response permutation procedure (MRPP)
    mrp # delta is the test statistic and A the effect size
    hist(mrp$boot.deltas,main="Random deltas") # Plot histogram delta-values of the permutations
    meandist(disCN,ind.CN$mod) # Finds pairwise mean dissimilarities among groups
    summary(meandist(disCN,ind.CN$mod)) # reports mean distances within and between groups

    disPC<-vegdist(i.sp_networks[[2]],method="bray",binary=TRUE)
    mrp2<-mrpp(disPC,ind_list[[2]]$mod,permutations=1000)
    mrp2
    hist(mrp2$boot.deltas,main="Random deltas")
    meandist(disPC,ind_list[[2]]$mod)
    summary(meandist(disPC,ind_list[[2]]$mod))

# Phenology as driver of modularity -----------------------------------------------------------------------------------------------------------

    phe.tot2<-list()
    phe.tot2[[1]]<-as.data.frame.matrix(phe.tot[[1]][,-7])
    phe.tot2[[2]]<-as.data.frame.matrix(phe.tot[[2]])
    phe.tot2 # Phenological composition of modules in empirical i-sp networks

  # Randomization test of independence:
    perms<-list()
    for(i in 1:length(phe.tot2)){
        perms[[i]]<-permatfull(phe.tot2[[i]],fixedmar="both",mtype="count",shuffle="ind",times=999) # column and row marginals fixed
    }
    chi.perms<-list()
    for(n in 1:length(perms)){
        chi.perm<-rep(NA,999)
        for(i in 1:length(perms[[n]]$perm)){
            chi.perm[i]<-chisq.test(perms[[n]]$perm[[i]])$statistic
        }
        chi.perms[[n]]<-chi.perm # null distribution of the statistic for the permutations. Compare the observed value with the permutations
    }
    chi.perms

    hist(chi.perms[[1]]) # Histogram statistic distribution under the null hypotheses.
    hist(chi.perms[[2]])

    # Count the number of times (N) the statistic in the null distribution is equal to or greater than the statistic for
    # the observed data. p-value is calculated as: P=N/n+1, where n is the number of permutations.
    pval<-list()
    for(i in 1:length(chi.perms)){
        pval[[i]]<- 1/1000 + sum(chi.perms[[i]] >= chisq.test(phe.tot2[[i]])$statistic)/1000 # p-value
    }
    pval


# (4) ASSOCIATION OF NODE FEATURES WITH TOPOLOGICAL ROLES  #####################################################################################

# See different roles of nodes in empirical i-sp networks -------------------------------------------------------------------------------------

    nh<-list()
    mh<-list()
    p<-list()
    c<-list()
    for (i in 1:length(roles)){
        nh[[i]]<-subset(roles[[i]],roles[[i]]$role=="NH")
        mh[[i]]<-subset(roles[[i]],roles[[i]]$role=="MH")
        p[[i]]<-subset(roles[[i]],roles[[i]]$role=="P")
        c[[i]]<-subset(roles[[i]],roles[[i]]$role=="C")
    }
    nh # Network hubs are important for the coherence of the module and the network.
    mh # Module hubs are important to coherence of its own module.
    p # Peripheral species have few links inside its module and rarely any to other modules.
    c # Connectors glue modules together.

# Which are the roles of plant pollen types? ------------------------------------------------------------------------------------------------

    plant_list[[1]]<-plant_list[[1]][-34,] # Remove isolated pollen type in CN
    plant_list[[1]]$role<-factor(plant_list[[1]]$role)

    subset(plant_list[[1]],plant_list[[1]]$role=="NH")
    subset(plant_list[[2]],plant_list[[2]]$role=="NH")

    subset(plant_list[[1]],plant_list[[1]]$role=="MH")
    subset(plant_list[[2]],plant_list[[2]]$role=="MH")

    subset(plant_list[[1]],plant_list[[1]]$role=="C")
    subset(plant_list[[2]],plant_list[[2]]$role=="C")

    subset(plant_list[[1]],plant_list[[1]]$role=="P")
    subset(plant_list[[2]],plant_list[[2]]$role=="P")

# Calculate average plant biological features for each role  ------------------------------------------------------------------------------

    mean.flowering<-list()
    mean.lsp<-list()
    mean.peak<-list()
    mean.abun<-list()
    for(i in 1:length(plant_list)){
        mean.flowering[[i]]<-tapply(plant_list[[i]]$pheno_weeks,plant_list[[i]]$role,mean)
        mean.lsp[[i]]<-tapply(plant_list[[i]]$Lsp,plant_list[[i]]$role,mean)
        mean.peak[[i]]<-tapply(plant_list[[i]]$pheno_peak,plant_list[[i]]$role,table)
        mean.abun[[i]]<-tapply(plant_list[[i]]$pollen_abun,plant_list[[i]]$role,mean)
    }
    mean.flowering
    mean.lsp
    mean.peak
    mean.abun

    sd.flowering<-list()
    sd.lsp<-list()
    sd.peak<-list()
    sd.abun<-list()
    for(i in 1:length(plant_list)){
        sd.flowering[[i]]<-tapply(plant_list[[i]]$pheno_weeks,plant_list[[i]]$role,sd)
        sd.lsp[[i]]<-tapply(plant_list[[i]]$Lsp,plant_list[[i]]$role,sd)
        sd.abun[[i]]<-tapply(plant_list[[i]]$pollen_abun,plant_list[[i]]$role,sd)
    }
    sd.flowering
    sd.lsp
    sd.abun

# Box-plot of biological plant features --------------------------------------------------------------------------------------------------------

    plant_roles<-read.table(file="plant_prop_list.txt",sep=",",header=T, na.strings="") # Dataframe of plant pollen types (CN and PC site together)
    plant_roles<-plant_roles[-34,]
    plant_roles$pheno_peak<-factor(plant_roles$pheno_peak,levels=c("may","june","july","august"))
    plant_roles$role<-factor(plant_roles$role,levels=c("P","C","MH","NH"))
    plant_roles$module<-as.factor(plant_roles$module)

    op <- par(mfrow=c(1,3))
    op[[1]]<-boxplot(pheno_weeks~role,data=plant_roles,ylab="Flowering duration (weeks)",xlab="Role")
    op[[2]]<-boxplot(Lsp~role,data=plant_roles,ylab="Links with species",xlab="Role")
    op[[3]]<-boxplot(log(pollen_abun)~role,data=plant_roles,ylab="Log(Pollen abundance)",xlab="Role")

# Test for differences in features among plant roles (Kruskal-Wallis test with Bonferroni correction) ------------------------------------------

    kruskal(plant_roles$pheno_weeks,plant_roles$role,p.adj="bonferroni")
    kruskal(plant_roles$pollen_abun,plant_roles$role,p.adj="bonferroni")
    kruskal(plant_roles$Lsp,plant_roles$role,p.adj="bonferroni")
    kruskal(plant_roles$Li,plant_roles$role,p.adj="bonferroni")

# Fisher exact test for differences in plant pheno_peak ----------------------------------------------------------------------------------------

    fisher.test(plant_roles$role,plant_roles$pheno_peak,simulate.p.value=TRUE, B=1000)
    table(plant_roles$role,plant_roles$pheno_peak)
    likelihood.test(plant_roles$role,plant_roles$pheno_peak,conservative=TRUE)$expected

# Multinomial logit model ---------------------------------------------------------------------------------------------------------------------

    plant_list2<-cbind(plant_roles,log(plant_roles$pollen_abun,base=10)) # log-transform variable
    names(plant_list2)[13]<-"log_pollen_abun"
    mdata<-mlogit.data(plant_list2,choice="role",shape="wide") # convert data from wide to long format
    mlogit.model<-mlogit(role~1|pheno_weeks+log_pollen_abun, alt.subset=c("P","NH","MH"),data=mdata,reflevel="P")
    plant_list2<-subset(plant_list2,plant_list2$role!="C") # remove connectors
    plant_list2$role<-factor(plant_list2$role,levels=c("P","MH","NH"))
    test1<-multinom(role~pheno_weeks+log_pollen_abun,data=plant_list2) # same model as before, another way of do it
    summary(mlogit.model) # Results logit model
    summary(test1)
    exp(coef(mlogit.model)) # Indicates the change in odds resulting from a unit change in the predictor
    exp(coef(test1))        # odds ratio >1
    plot(allEffects(test1))
    effect("pheno_weeks",test1,default.levels=14)$prob # see changes in probability for each role associated with changes in phenology duration
    effect("log_pollen_abun",test1,default.levels=6)$prob # see changes in probability for each role associated with changes in log(pollen_abun)

# Plot multinomial logit model results ---------------------------------------------------------------------------------------------------------

    probs<-effect("pheno_weeks",test1,default.levels=14)$prob
    colnames(probs)<-c("P","MH","NH")
    probs<-melt(probs)
    names(probs)<-c("pheno_weeks","role","probability")
    ggplot(probs,aes(x=pheno_weeks,y=probability,colour=role))+geom_line(size=1)+
    xlab("Flowering duration (weeks)")+ylab("Fitted probability")+scale_y_continuous(breaks=seq(0, 1, 0.1))+scale_x_continuous(breaks=seq(0, 14, 2))+ # This is like an effect plot
    theme(axis.title.y=element_text(vjust=0.2),axis.title.x=element_text(vjust=0.1))

    probs2<-effect("log_pollen_abun",test1,default.levels=6)$prob
    colnames(probs2)<-c("P","MH","NH")
    probs2<-melt(probs2)
    names(probs2)<-c("log_pollen_abun","role","probability")
    dev.new()
    ggplot(probs2,aes(x=log_pollen_abun,y=probability,colour=role))+geom_line(size=1)+
    xlab("Log (pollen abundance)")+ylab("Fitted probability")+scale_y_continuous(breaks=seq(0, 1, 0.1))+scale_x_continuous(breaks=seq(0, 6, 1))+ theme(axis.title.y=element_text(vjust=0.2),axis.title.x=element_text(vjust=0.1))

# Which are the roles of individuals? ----------------------------------------------------------------------------------------------------------

    ind_list[[1]]<-ind_list[[1]][-25,] # Remove isolated individual in CN
    ind_list[[1]]$role<-factor(ind_list[[1]]$role)
    ind_list[[2]]$role<-factor(ind_list[[2]]$role)

    subset(ind_list[[1]],ind_list[[1]]$role=="C")
    subset(ind_list[[2]],ind_list[[2]]$role=="C")

    subset(ind_list[[1]],ind_list[[1]]$role=="P")
    subset(ind_list[[2]],ind_list[[2]]$role=="P")

# Test for differences in biological features among individual roles (Kruskal-Wallis test with Bonferroni correction) --------------------------

    ind_roles<-read.table(file="ind_prop_list.txt",sep=",",header=T,na.strings="") # Table with individuals from both sites
    ind_roles<-ind_roles[-25,] # remove isolated individual
    ind_roles$phenol<-factor(ind_roles$phenol,levels=c("may","june","july","august"))
    ind_roles$role<-factor(ind_roles$role,levels=c("P","C"))
    ind_roles$sex<-factor(ind_roles$sex,levels=c("f","m"))

    kruskal(ind_roles$Li,ind_roles$role,p.adj="bonferroni")
    kruskal(ind_roles$Lsp,ind_roles$role,p.adj="bonferroni")
    kruskal(ind_roles$no_weeks,ind_roles$role,p.adj="bonferroni")
    kruskal(ind_roles$abun,ind_roles$role,p.adj="bonferroni")

# Fisher exact test for differences in individual phenophase -----------------------------------------------------------------------------------

    fisher.test(ind_roles$role,ind_roles$phenol,simulate.p.value=TRUE, B=1000)
    table(ind_roles$role,ind_roles$phenol)
    likelihood.test(ind_roles$role,ind_roles$phenol,conservative=TRUE)
    table(ind_roles$role,ind_roles$sex)
    likelihood.test(ind_roles$role,ind_roles$sex,conservative=TRUE)
    fisher.test(ind_roles$role,ind_roles$sex,simulate.p.value=TRUE, B=1000)

# Boxplot of individual features among topological roles -----------------------------------------------------------------------

    op <- par(mfrow=c(1,3))
    op[[1]]<- boxplot(Li~role,data=ind_roles,ylab="Individual linkage level",xlab="Role")
    op[[2]]<- boxplot(no_weeks~role,data=ind_roles,ylab="Species phenophase length",xlab="Role")
    op[[3]]<- boxplot(abun~role,data=ind_roles,ylab="Abundance",xlab="Role")

# Logistic model ---------------------------------------------------------------------------------------------------------------

    table(ind_roles$role,ind_roles$phenol) # There aren't connectors in may, so before running the analysis we must define new phenological categories
    pheno.cat <- vector(length=length(ind_roles$phenol)) # Classify into only two phenological categories (early season, late season)
    for (i in 1:length(ind_roles$phenol)) {
        pheno.cat[i] <- if (ind_roles$phenol[i]=="may") 1 else if (ind_roles$phenol[i]=="july") 2 else if (ind_roles$phenol[i]=="june") 1 else 2
    }
    pheno.cat
    ind_roles<-cbind(ind_roles,pheno.cat)
    ind_roles$pheno.cat<-factor(ind_roles$pheno.cat)

  # Built a model with many variables and delete variables sequentially to select the best simple model:
    model1<-glm(role~Li+pheno.cat+abun+no_weeks+Li*pheno.cat+abun*pheno.cat,data=ind_roles,family=binomial(link="logit"))
    drop1(model1) # backward simplification: delete predictors which give a model with a smaller AIC
    step(model1)

    modelb<-glm(formula = role ~ Li + pheno.cat + Li:pheno.cat, family = binomial(link = "logit"),data=ind_roles)
    summary(modelb)
    plot(allEffects(modelb))

    y<-data.frame(pheno.cat=factor(c(1,2)),Li=mean(ind_roles$Li)) # Simulate a small dataset to see the effect of predictors
    predict(modelb,newdata=y,type="response")
    effect("Li",modelb,default.levels=13) # changes in probability associated with increasing linkage level
    m<-data.frame(pheno.cat=factor(rep(c(1,2),each=13)),Li=rep(c(1:13),2)) # see effects of increasing Li within each phenol
    pp<-cbind(m,predict(modelb,newdata=m,type="response",se=FALSE))
    by(pp[,3],pp$pheno.cat,mean)

# Plot logit model results -------------------------------------------------------------------------------------------------------------

    names(pp)<-c("phenophase","Li","probability")
    ggplot(pp,aes(x=Li,y=probability,colour=phenophase))+geom_line(size=1)+
    xlab("Individual linkage level")+ylab("Fitted probability")+scale_y_continuous(breaks=seq(0, 1, 0.1))+scale_x_continuous(breaks=seq(0, 13, 1))+
    theme(axis.title.y=element_text(vjust=0.2),axis.title.x=element_text(vjust=0.1))+
    scale_colour_discrete(name  ="Phenophase",labels=c("may-june", "july-august"))

