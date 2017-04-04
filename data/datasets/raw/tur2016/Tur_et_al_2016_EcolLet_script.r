################################################################################################
##                R code for analysis included in article:                                    ##
##                C. Tur, A. Sáez, A. Traveset & M.A. Aizen (2016)                            ##
## Evaluating the effects of pollinator-mediated interactions using pollen transfer networks: ##
##        evidence of widespread facilitation in south Andean plant communities               ##
##                             Ecology Letters                                                ##
################################################################################################

# This script was written by Cristina Tur, who can be contacted for further information in:
# cris.tur.espinosa@gmail.com

# Load R packages --------------------------------------------------------------------------------------------------

  library(lme4)
  library(ggplot2)
  library(grid)
  library(igraph)

# Load and prepare data for analyses ------------------------------------------------------------------------------
# Remember to first download the file "Data_Tur_et_al_2016_EcolLet.txt" and save it in the working directory.

  database<-read.table(file="Data_Tur_et_al_2016_EcolLet.txt",sep=";",header=TRUE) # Dataframe with data of pollen deposition on stigmas
  database$altitude<-as.factor(database$altitude)
  database$date<-factor(database$date,levels=c("17-dic-11","20-dic-11","31-dic-11", "04-ene-11", "03-ene-11","17-ene-11", "20-ene-11",
          "21-ene-11", "01-feb-11", "02-feb-11", "05-feb-11", "14-feb-11","17-feb-11", "03-mar-11" )) # order the factor
  Dec_s1<-c("17-dic-11","20-dic-11")
  Jan_s2<-c("31-dic-11","04-ene-11","03-ene-11")
  Jan_s3<-c("17-ene-11","20-ene-11","21-ene-11")
  Feb_s4<-c("01-feb-11","02-feb-11","05-feb-11")
  Feb_s5<-c("14-feb-11","17-feb-11")
  Mar_s6<-c("03-mar-11")
  levels(database$date)[levels(database$date) %in% Dec_s1] <- "Dec_s1" # reassign values to a new factor with six levels
  levels(database$date)[levels(database$date) %in% Jan_s2] <- "Jan_s2"
  levels(database$date)[levels(database$date) %in% Jan_s3] <- "Jan_s3"
  levels(database$date)[levels(database$date) %in% Feb_s4] <- "Feb_s4"
  levels(database$date)[levels(database$date) %in% Feb_s5] <- "Feb_s5"
  levels(database$date)[levels(database$date) %in% Mar_s6] <- "Mar_s6"

  head(database)
  str(database)
  cp<-subset(database,as.character(database$receptor)==as.character(database$donor))  # subset conspecific depositions
  hp<-subset(database,as.character(database$receptor)!=as.character(database$donor))  # subset heterospecific depositions
  hp.grains<-aggregate(hp$total,by=list(hp$code),FUN=sum) # total HP deposition per stigma
  names(hp.grains)<-c("code","HP") # rename variables
  deposition<-merge(cp,hp.grains,by="code",all.x=TRUE) # merge both dataframes
  deposition$HP[is.na(deposition$HP)]<-0 # replace NA values with 0
  deposition<-deposition[,-7] # delete column
  names(deposition)[7]<-"CPgerm";names(deposition)[8]<-"CPnon.germ";names(deposition)[9]<-"CP" # rename variables
  total.grains<-deposition$CP+deposition$HP # total pollen load per stigma
  deposition<-cbind(deposition,total.grains) # add the total number of grains to the dataframe
  deposition # Dataframe with pollen deposition per stigma sampled


# MODEL 1 - Poisson GLMM relation CP vs. HP -------------------------------------------------------------------

  dep<-split(deposition,deposition$altitude)
  for(i in 1:length(dep)){
  total.hp<-tapply(dep[[i]]$HP,dep[[i]]$receptor,sum) # total HP grains per species
  sp<-names(which(total.hp==0)) # species without hp deposition
    pos<-c()
    for(x in 1:length(sp)){
        p<-which(dep[[i]]$receptor==sp[x]) # select observations for species with HP total=0
        pos<-c(pos,p)
    }
  dep[[i]]<-dep[[i]][-pos,] # remove observations
  dep[[i]]$receptor<-factor(dep[[i]]$receptor)
  dep[[i]]$date<-factor(dep[[i]]$date)
  }
  dep

  poisson.m1<-list()
  poisson.m1.coefs<-list()
  for(i in 1:length(dep)){
      pois.mod<-lmer(CP~HP+date+ (1+HP|receptor)+(1|plant),data=dep[[i]],REML=TRUE,family=poisson)
      poisson.m1[[i]]<-summary(pois.mod) # store model summary
      qq <- attr(ranef(pois.mod, postVar = TRUE)[[2]], "postVar") # Extract the variances of the random effects of receptor
      error<-sqrt(qq[2,2,])
      coefs<-coef(pois.mod)$receptor
      slopes<-data.frame(coefs,error)
      slopes<-slopes[order(slopes$HP,decreasing=T),]
      species <- factor(rownames(slopes), levels = rownames(slopes)[order(slopes$HP,decreasing=T)]) # sort species names. This is needed for the plot
      poisson.m1.coefs[[i]]<-slopes
      dev.new(height=6,width=7)
      print(
          ggplot(slopes, aes(x=species, y=HP)) +
          geom_errorbar(aes(ymin=slopes$HP-2*(slopes$error), ymax=slopes$HP+2*(slopes$error)), width=0,col="gray25") +
          geom_hline(yintercept=0,linetype="dashed",width=2)+
          geom_point(col="gray25")+ylim(-0.3,0.6)+
          theme_classic() +
          xlab("")+ ylab("Slope")+
          ggtitle(paste(names(dep)[i],"m",sep=" "))+theme(axis.title.x=element_text(size=14,vjust=0),axis.title.y=element_text(size=14,vjust=0))+
          theme(panel.grid= element_blank(),axis.text.x=element_text(size=12,hjust=1,vjust=0.25,angle=90),axis.text.y=element_text(size=14),plot.margin=unit(c(3,3,2,2),"lines"))+
          theme(panel.border=element_rect(colour="black",fill=NA),plot.title=element_text(vjust=4))
          )
  }
  poisson.m1
  poisson.m1.coefs # Model 1 results

  m1.sign<-matrix(nrow=3,ncol=3)
  for(i in 1:length(dep)){
    interval.inf<-poisson.m1.coefs[[i]]$HP-2*poisson.m1.coefs[[i]]$error
    interval.sup<-poisson.m1.coefs[[i]]$HP+2*poisson.m1.coefs[[i]]$error
    neutral<-length(which(interval.inf < 0&0 < interval.sup))
    positive<-length(which(interval.inf > 0&0 < interval.sup))
    negative<-length(which(interval.inf < 0&0 > interval.sup))
    m1.sign[,i]<-c(positive,neutral,negative)/nrow(poisson.m1.coefs[[i]])
    dev.new() # Pie charts
    pie(m1.sign[,i],col=c("green","orange","red"),labels=paste(round(m1.sign[,i]*100,2),"%"))
  }
  rownames(m1.sign)<-c("positive","neutral","negative")
  colnames(m1.sign)<-c("1600 m", "1800 m", "2000 m")
  m1.sign # Table showing the proportion of species with each type of effect


# MODEL 2 - Binomial GLMMs relation CP germ ratio vs. HP ----------------------------------------------------

  binom.m2<-list()
  binom.m2.coefs<-list()
  for(i in 1:length(dep)){
      binom.mod<-lmer(cbind(CPgerm,CPnon.germ)~HP+date+ (1+HP|receptor),data=dep[[i]],REML=TRUE,family=binomial)
      binom.m2[[i]]<-summary(binom.mod) # store summary
      qq <- attr(ranef(binom.mod, postVar = TRUE)[[1]], "postVar") # Extract the variances of the random effects
      error<-sqrt(qq[2,2,])
      coefs<-coef(binom.mod)$receptor
      slopes<-data.frame(coefs,error)
      slopes<-slopes[order(slopes$HP,decreasing=T),]
      species <- factor(rownames(slopes), levels = rownames(slopes)[order(slopes$HP,decreasing=T)]) # sort species names
      binom.m2.coefs[[i]]<-slopes
      dev.new(height=6,width=7)
      print(
          ggplot(slopes, aes(x=species, y=HP)) +
          geom_errorbar(aes(ymin=slopes$HP-2*(slopes$error), ymax=slopes$HP+2*(slopes$error)), width=0,col="gray25") +
          geom_hline(yintercept=0,linetype="dashed",width=2)+
          geom_point(col="gray25")+ylim(-0.8,1)+
          theme_classic() +
          xlab("")+ ylab("Slope")+
          ggtitle(paste(names(dep)[i],"m",sep=" "))+theme(axis.title.x=element_text(size=14,vjust=0),axis.title.y=element_text(size=14,vjust=0))+
          theme(panel.grid= element_blank(),axis.text.x=element_text(size=12,hjust=1,vjust=0.25,angle=90),axis.text.y=element_text(size=14),plot.margin=unit(c(3,3,2,2),"lines"))+
          theme(panel.border=element_rect(colour="black",fill=NA),plot.title=element_text(vjust=4))
          )
  }
  binom.m2
  binom.m2.coefs # Model 2 results

  m2.sign<-matrix(nrow=3,ncol=3)
  for(i in 1:length(dep)){
    interval.inf<-binom.m2.coefs[[i]]$HP-2*binom.m2.coefs[[i]]$error
    interval.sup<-binom.m2.coefs[[i]]$HP+2*binom.m2.coefs[[i]]$error
    neutral<-length(which(interval.inf < 0&0 < interval.sup))
    positive<-length(which(interval.inf > 0&0 < interval.sup))
    negative<-length(which(interval.inf < 0&0 > interval.sup))
    m2.sign[,i]<-c(positive,neutral,negative)/nrow(binom.m2.coefs[[i]])
    dev.new() # Pie charts
    pie(m2.sign[,i],col=c("green","orange","red"),labels=paste(round(m2.sign[,i]*100,2),"%"))
  }
  rownames(m2.sign)<-c("positive","neutral","negative")
  colnames(m2.sign)<-c("1600 m", "1800 m", "2000 m")
  m2.sign # Table with the proportion of species with each type of effect


# MODEL 3 - Poisson model relation CP vs. HP with a combination of 2 random effects -----------------------------------------------------

  cp # Dataframe with conspecific depositions
  hp # Dataframe with heterospecific depositions
  ipt<-merge(hp,cp,by="code",all.x=TRUE) # Merge both dataframes
  ipt<-ipt[,-c(8,9,11:16)] # Select variables of interest
  names(ipt)<-c("code","altitude","date","plant","stigma","receptor","donor","HP","CPgerm","CPnon.germ","CP") # rename variables
  ipt

  dep2<-split(ipt,ipt$altitude)
  for(i in 1:length(dep2)){
  dep2[[i]]$receptor<-factor(dep2[[i]]$receptor)
  dep2[[i]]$date<-factor(dep2[[i]]$date)
  dep2[[i]]$donor<-factor(dep2[[i]]$donor)
  }
  dep2

  poisson.m3<-list()
  poisson.m3.coefs<-list()
  for(i in 1:length(dep2)){
      pois.mod<-lmer(CP~HP+date+(1+HP|receptor)+(1+HP|donor:receptor),data=dep2[[i]],REML=TRUE,family=poisson)
      poisson.m3[[i]]<-summary(pois.mod) # store summary
      slopes.randoms<-list()
      for(j in 1:2){
      qq <- attr(ranef(pois.mod, postVar = TRUE)[[j]], "postVar") # Extract the variances of the random effects of: (1) each interaction, (2) Receptor
      error<-sqrt(qq[2,2,])
      coefs<-coef(pois.mod)[[j]]
      slopes<-data.frame(coefs,error)
      slopes<-slopes[order(slopes$HP,decreasing=T),]
      slopes.randoms[[j]]<-slopes
  }
  poisson.m3.coefs[[i]]<-slopes.randoms
  }
  poisson.m3
  poisson.m3.coefs # Model 3 results

  m3.sign<-matrix(nrow=3,ncol=3)
  for(i in 1:length(dep2)){
    interval.inf<-poisson.m3.coefs[[i]][[1]]$HP-2*poisson.m3.coefs[[i]][[1]]$error
    interval.sup<-poisson.m3.coefs[[i]][[1]]$HP+2*poisson.m3.coefs[[i]][[1]]$error
    neutral<-length(which(interval.inf < 0&0 < interval.sup))
    positive<-length(which(interval.inf > 0&0 < interval.sup))
    negative<-length(which(interval.inf < 0&0 > interval.sup))
    m3.sign[,i]<-c(positive,neutral,negative)/nrow(poisson.m3.coefs[[i]][[1]])
    rownames(m3.sign)<-c("positive","neutral","negative")
    colnames(m3.sign)<-c("1600 m", "1800 m", "2000 m")
    dev.new() # Pie charts
    pie(m3.sign[,i],col=c("green","orange","red"),labels=paste(round(m3.sign[,i]*100,2),"%"))
  }
  m3.sign # Table with proportion of donor:receptor interactions with each type of effect

  m3.int<-list()
     for(i in 1:length(dep2)){
      interval.inf<-poisson.m3.coefs[[i]][[1]]$HP-2*poisson.m3.coefs[[i]][[1]]$error
      interval.sup<-poisson.m3.coefs[[i]][[1]]$HP+2*poisson.m3.coefs[[i]][[1]]$error
      neutral<-poisson.m3.coefs[[i]][[1]][which(interval.inf < 0&0 < interval.sup),]
      positive<-poisson.m3.coefs[[i]][[1]][which(interval.inf > 0&0 < interval.sup),]
      negative<-poisson.m3.coefs[[i]][[1]][which(interval.inf < 0&0 > interval.sup),]
      m3.int[[i]]<-data.frame(rbind(positive, neutral,negative),effect=c(rep("positive",length(which(interval.inf > 0&0 < interval.sup))),
                rep("neutral",length(which(interval.inf < 0&0 < interval.sup))),rep("negative",length(which(interval.inf < 0&0 > interval.sup)))))
      }
  m3.int # List with model coefficients, intercepts and effect (sign) for all donor:receptor interactions


# MODEL 4 - Binomial model proportion of germinated CP vs. HP with a combination of 2 random effects -------------------------------------

  binom.m4<-list()
  binom.m4.coefs<-list()
  for(i in 1:length(dep2)){
      binom.mod<-lmer(cbind(CPgerm,CPnon.germ)~HP+date+(1+HP|receptor)+(1+HP|donor:receptor),data=dep2[[i]],REML=TRUE,family=binomial)
      binom.m4[[i]]<-summary(binom.mod) # store summary
      slopes.randoms<-list()
      for(j in 1:2){
      qq <- attr(ranef(binom.mod, postVar = TRUE)[[j]], "postVar") # Extract the variances of the random effects of: (1) each interaction, (2) Receptor
      error<-sqrt(qq[2,2,])
      coefs<-coef(binom.mod)[[j]]
      slopes<-data.frame(coefs,error)
      slopes<-slopes[order(slopes$HP,decreasing=T),]
      slopes.randoms[[j]]<-slopes
  }
  binom.m4.coefs[[i]]<-slopes.randoms
  }
  binom.m4
  binom.m4.coefs # Model 4 results

  m4.sign<-matrix(nrow=3,ncol=3)
  for(i in 1:length(dep2)){
    interval.inf<-binom.m4.coefs[[i]][[1]]$HP-2*binom.m4.coefs[[i]][[1]]$error
    interval.sup<-binom.m4.coefs[[i]][[1]]$HP+2*binom.m4.coefs[[i]][[1]]$error
    neutral<-length(which(interval.inf < 0&0 < interval.sup))
    positive<-length(which(interval.inf > 0&0 < interval.sup))
    negative<-length(which(interval.inf < 0&0 > interval.sup))
    m4.sign[,i]<-c(positive,neutral,negative)/nrow(binom.m4.coefs[[i]][[1]])
    rownames(m4.sign)<-c("positive","neutral","negative")
    colnames(m4.sign)<-c("1600 m", "1800 m", "2000 m")
    dev.new() # Pie charts
    pie(m4.sign[,i],col=c("green","orange","red"),labels=paste(round(m4.sign[,i]*100,2),"%"))
  }
  m4.sign # Table with proportion of donor:receptor interactions with each type of effect

  m4.int<-list()
     for(i in 1:length(dep2)){
      interval.inf<-binom.m4.coefs[[i]][[1]]$HP-2*binom.m4.coefs[[i]][[1]]$error
      interval.sup<-binom.m4.coefs[[i]][[1]]$HP+2*binom.m4.coefs[[i]][[1]]$error
      neutral<-binom.m4.coefs[[i]][[1]][which(interval.inf < 0&0 < interval.sup),]
      positive<-binom.m4.coefs[[i]][[1]][which(interval.inf > 0&0 < interval.sup),]
      negative<-binom.m4.coefs[[i]][[1]][which(interval.inf < 0&0 > interval.sup),]
      m4.int[[i]]<-data.frame(rbind(positive, neutral,negative),effect=c(rep("positive",length(which(interval.inf > 0&0 < interval.sup))),
                rep("neutral",length(which(interval.inf < 0&0 < interval.sup))),rep("negative",length(which(interval.inf < 0&0 > interval.sup)))))
      }
   m4.int # List with model coefficients, intercepts and effect (sign) for all donor:receptor interactions


# Network analyses ------------------------------------------------------------------------------------------

  edges<-aggregate(database$total,by=list(database$altitude,database$receptor,database$donor),FUN=sum)
  names(edges)<-c("altitude","receptor","donor","no.grains") # Change variable names
  edges<-subset(edges,edges$no.grains!=0) # Remove rows with 0 pollen grains
  edge.list<-list()
  edge.list[[1]]<-subset(edges,altitude=="1600",select=c(donor,receptor)) # Subset species at each altitudinal level
  edge.list[[2]]<-subset(edges,altitude=="1800",select=c(donor,receptor)) # 1st column= From; 2nd column= To
  edge.list[[3]]<-subset(edges,altitude=="2000",select=c(donor,receptor))
  edge.list # List with pollen transfers for each altitudinal transect

  g<-list()
  for(i in 1:length(edge.list)){
      g[[i]]<-graph.data.frame(edge.list[[i]],directed=TRUE) # Create graph object from edge list
  }
  g[[1]]<- set.graph.attribute(g[[1]], "name", "1600.pollen.transfer.network") # put a name to the networks
  g[[2]]<- set.graph.attribute(g[[2]], "name", "1800.pollen.transfer.network")
  g[[3]]<- set.graph.attribute(g[[3]], "name", "2000.pollen.transfer.network")
  g # Gives information on the type of graph (directed, named), no. nodes and interactions

  iptransfers<-c()
  for(i in 1:length(g)){
      iptransfers[i]<-ecount(simplify(g[[i]],remove.loops=TRUE))
  }
  iptransfers # number of interspecific pollen transfers for each altitudinal network

  in.degree<-list()
  out.degree<-list()
  for(i in 1:length(g)){
    in.degree[[i]]<-degree(g[[i]],mode="in",loops=FALSE)
    in.degree[[i]]<-in.degree[[i]][sort(names(in.degree[[i]]))]
    in.degree[[i]]<-in.degree[[i]][c(match(unique(edge.list[[i]]$receptor),names(in.degree[[i]])))] # exclude plants without stigmas sampled in the field
    out.degree[[i]]<-degree(g[[i]],mode="out",loops=FALSE)
    out.degree[[i]]<-out.degree[[i]][sort(names(out.degree[[i]]))]
    out.degree[[i]]<-out.degree[[i]][c(match(unique(edge.list[[i]]$receptor),names(out.degree[[i]])))]
  }
  in.degree # IN-degree: from how many other species receives pollen? (excluding conspecific loop links)
  out.degree # OUT-degree: to how many other species donates pollen? (excluding conspecific loop links)
  
  dev.new(height=7.7,width=8.5) # Plot degree distributions IN- and OUT-
  op <- par(mfrow=c(2,3),mar=c(5,3,1,1),oma=c(2,3,1,1))
  op[[1]]<-hist(in.degree[[1]],freq=FALSE,breaks=seq(from=0,to=20,by=1),xlim=c(0,20),ylim=c(0,0.7),col=rgb(0,1,0,1),border="white",main="",xlab="",ylab="",cex.lab=1.5,axes=F,cex.main=1.5)
           axis(side=1,at=seq(0,20,2),labels=TRUE,cex.axis=1.2); axis(side=2,at=seq(0,0.7,0.1),labels=TRUE,cex.axis=1.2)
  op[[2]]<-hist(in.degree[[2]],freq=FALSE,breaks=seq(from=0,to=20,by=1),xlim=c(0,20),ylim=c(0,0.7),col=rgb(1,0,0,1),border="white",main="",xlab="IN-degree",ylab="",cex.lab=1.8,axes=F,cex.main=1.5)
           axis(side=1,at=seq(0,20,2),labels=TRUE,cex.axis=1.2); axis(side=2,at=seq(0,0.7,0.1),labels=TRUE,cex.axis=1.2)
  op[[3]]<-hist(in.degree[[3]],freq=FALSE,breaks=seq(from=0,to=20,by=1),xlim=c(0,20),ylim=c(0,0.7),col=rgb(0,0,1,1),border="white",main="",xlab="",ylab="",cex.lab=1.5,axes=F,cex.main=1.5)
           axis(side=1,at=seq(0,20,2),labels=TRUE,cex.axis=1.2) ; axis(side=2,at=seq(0,0.7,0.1),labels=TRUE,cex.axis=1.2)
  op[[4]]<-hist(out.degree[[1]],freq=FALSE,breaks=seq(from=0,to=20,by=1),xlim=c(0,20),ylim=c(0,0.7),col=rgb(0,1,0,1),border="white",main="",xlab="",ylab="",cex.lab=1.5,axes=F)
           axis(side=1,at=seq(0,20,2),labels=TRUE,cex.axis=1.2); axis(side=2,at=seq(0,0.7,0.1),labels=TRUE,cex.axis=1.2)
  op[[5]]<-hist(out.degree[[2]],freq=FALSE,breaks=seq(from=0,to=20,by=1),xlim=c(0,20),ylim=c(0,0.7),col=rgb(1,0,0,1),border="white",main="",xlab="OUT-degree",ylab="",cex.lab=2,axes=F)
           axis(side=1,at=seq(0,20,2),labels=TRUE,cex.axis=1.2); axis(side=2,at=seq(0,0.7,0.1),labels=TRUE,cex.axis=1.2)
  op[[6]]<-hist(out.degree[[3]],freq=FALSE,breaks=seq(from=0,to=20,by=1),xlim=c(0,20),ylim=c(0,0.7),col=rgb(0,0,1,1),border="white",main="",xlab="",ylab="",cex.lab=1.8,axes=F)
           axis(side=1,at=seq(0,20,2),labels=TRUE,cex.axis=1.2) ; axis(side=2,at=seq(0,0.7,0.1),labels=TRUE,cex.axis=1.2)
  mtext("Relative frequency", cex=1.3, side=2, line=1, outer=T)

  correlation<-list()
  roles<-list() # Assign a role to each species. Factor with 3 levels= donor, receipt, donor-receipt
  nroles<-list()
  dev.new(width=7.5,height=3.5)
  op <- par(mfrow=c(1,3),mar=c(3,3,1,1),oma=c(4,4,2,2))
  for(i in 1:length(in.degree)){
      correlation[[i]]<-cor.test(in.degree[[i]],out.degree[[i]],method=c("spearman"),exact=F)
      roles[[i]]<-ifelse(in.degree[[i]]>out.degree[[i]],"receipt","donor")
      roles[[i]][c(which(in.degree[[i]]==out.degree[[i]]&in.degree[[i]]!=0))]<-"donor-receipt"
      roles[[i]][c(which(in.degree[[i]]==out.degree[[i]]&in.degree[[i]]==0))]<-NA # exclude plants without donation and deposition
      roles[[i]]<-as.factor(roles[[i]])
      nroles[[i]]<-prop.table(table(roles[[i]]))
  }
  correlation # Correlation IN- and OUT-degree of species
  nroles # Proportion of species within each role

  dat <- data.frame()
  for(i in seq(along=in.degree)) for(j in names(in.degree[[i]]))
                                   dat[j,i] <- in.degree[[i]][j]
  dat # dataframe in-degree of species in all altitudinal levels
  s1<-dat[complete.cases(dat[,c(1,2)]),c(1,2)] # repeated species 1600-1800 m (20 spp)
  s2<-dat[complete.cases(dat[,c(1,3)]),c(1,3)] # repeated species 1600-2000 m (4 spp)
  s3<-dat[complete.cases(dat[,c(2,3)]),c(2,3)] # repeated species 1800-2000 m (5 spp)
  colnames(s1)<-c("in1","in2");colnames(s2)<-c("in1","in2");colnames(s3)<-c("in1","in2")
  rsp<-rbind(s1,s2,s3)
  cor.test(rsp[,1],rsp[,2],method=c("spearman"),exact=F) # Degree correlation IN-IN for species repeated in transects

  dat2 <- data.frame()
  for(i in seq(along=out.degree)) for(j in names(out.degree[[i]]))
                                   dat2[j,i] <- out.degree[[i]][j]
  dat2 # dataframe out-degree of species in all altitudinal levels
  s1<-dat2[complete.cases(dat2[,c(1,2)]),c(1,2)] # repeated species 1600-1800 m (20 spp)
  s2<-dat2[complete.cases(dat2[,c(1,3)]),c(1,3)] # repeated species 1600-2000 m (4 spp)
  s3<-dat2[complete.cases(dat2[,c(2,3)]),c(2,3)] # repeated species 1800-2000 m (5 spp)
  colnames(s1)<-c("out1","out2");colnames(s2)<-c("out1","out2");colnames(s3)<-c("out1","out2")
  rsp2<-rbind(s1,s2,s3)
  cor.test(rsp2[,1],rsp2[,2],method=c("spearman"),exact=F) # Degree correlation OUT-OUT for species repeated in transects


