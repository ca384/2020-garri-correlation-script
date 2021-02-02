# 2020-garri-correlation-script
Scripts for 2020 cassava roots and garri analysis
##scriot for correlation of garri traits

umu.oto<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020.umu.otobi.combine.harvestdata.csv") #harvest data for two locations

garri.38<- read.csv("~/OneDrive - Cornell University/2020 Data/2020coplete_umu_oto_garri_data.csv", header=TRUE)
dim(garri.38)
str(garri.38)
garri.38$genotypes.garri<-as.factor(garri.38$genotypes.garri)

garri.38$LOCATION<-as.factor(garri.38$LOCATION)

garri.38$garri<-round(log(garri.38$GARRI.YIELD.),3)

garri.38$garri.plot<-round(log(garri.38$garri.yield.plot), 3)

garri.38$peels<-round(garri.38$WT.OF.PEEL*100/garri.38$fresh.ROOT.WT.,3)

garri.38$peeled.root<-round(garri.38$WT.OF.PEELED.ROOT.*100/garri.38$fresh.ROOT.WT.,3)

garri.38$dewatered.mash<-round(garri.38$WT.OF.DEWATERED.MASH*100/garri.38$fresh.ROOT.WT.,3)

## plots

hist(garri.38$dewatered.mash)

hist(garri.38$peels)

hist(garri.38$garri)

hist(garri.38$garri.plot)

hist(garri.38$peeled.root)

hist(garri.38$dmcov)

hist(garri.38$dmc)

boxplot(garri.38$dmcov~garri.38$LOCATION)

boxplot(garri.38$garri.plot~garri.38$LOCATION)

boxplot(garri.38$dmc~garri.38$LOCATION, main="Dry matter content by specific gravity ",ylab = "DMC (%)",xlab="LOCATION")

##Removing outliers using the boxplot method
outliers <- boxplot(garri.38$dewatered.mash,plot=FALSE)$out  
#by setting plot to false the boxplot is hidden

garri.38[which(garri.38$dewatered.mash%in% outliers ),] 
#will bring out the rows and columns containing the outlier

m <- garri.38[-which(garri.38$dewatered.mash%in% outliers),] 
#will make a subset of those that are not outlier

boxplot(m$dewatered.mash~m$LOCATION,main="Proportion of dewatered mash per fresh weight",ylab = "Dewatered mash (%)",xlab="Location")

library(dplyr)

subset.garri.38<-select(garri.38,LOCATION,peels,peeled.root,dewatered.mash,dmc,dmcov,garri.plot,garri)

subset.garri.38 <- subset.garri.38 %>% rename(d.mash=dewatered.mash)

dim(subset.garri.38)

umu.38<-subset(subset.garri.38, LOCATION=="umudike")

umu.38$LOCATION<-NULL

oto.38<-subset(subset.garri.38, LOCATION=="OTOBI")

oto.38$LOCATION<-NULL

library(corrplot)

corr.umu<-round(cor(umu.38,use="complete.obs"),3)

corplot.umu<-corrplot(corr.umu, type = "upper", order = "hclust",tl.col = "black" , tl.srt = 90,  cl.pos = "n")

write.csv(corplot.umu,file="~/OneDrive - Cornell University/2020 Data/correlation.umu_new.csv")


corre.oto<-round(cor(oto.38,use="complete.obs"),3)
corplot.oto<-corrplot(corre.oto, type = "upper", order = "hclust",tl.col = "black" , tl.srt = 90,  cl.pos = "n")

write.csv(corplot.oto,file="~/OneDrive - Cornell University/2020 Data/correlation.oto_new.csv")

