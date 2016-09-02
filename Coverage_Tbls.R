## --------------------
## Name: Coverage_Tbls.R
## Program version: R 3.2.2
## Dependencies: plyr, xtable
## Author: J.H. Belle
## Purpose: Create table of mean coverage and population-weighted coverage by region, season and algorithm
## -------------------
library(plyr)
library(xtable)

# Read in data
All <- read.csv("H:/Rotation_Yang/AggDatDecade.csv")[,c(2,12,14,15,17,18,20)]
Winter <- read.csv("H:/Rotation_Yang/AggDatDecade12.csv")[,c(2,12,14,15,17,18,20)]
Spring <- read.csv("H:/Rotation_Yang/AggDatDecade3.csv")[,c(2,12,14,15,17,18,20)]
Summer <- read.csv("H:/Rotation_Yang/AggDatDecade6.csv")[,c(2,12,14,15,17,18,20)]
Fall <- read.csv("H:/Rotation_Yang/AggDatDecade9.csv")[,c(2,12,14,15,17,18,20)]
East <- read.csv("H:/Rotation_Yang/GridPopLakes2.csv")[,c(3,7:14)]


# Join everything together
AllKinds <- rbind.data.frame(All, Winter, Spring, Summer, Fall)
AllKinds$Season <- as.factor(c(rep("All", nrow(All)), rep("Winter", nrow(Winter)), rep("Spring", nrow(Spring)), rep("Summer", nrow(Summer)), rep("Fall", nrow(Fall))))
AllKinds2 <- merge(AllKinds, East, by.x="ID", by.y="Input_FID", all.x=T)
# Remove lake and ocean pixels
AllKinds <- AllKinds2[AllKinds2$Lake==0,]
# Create table
CalcRes2 <- function(x){
  Covs <- c(mean(x$PCovDT3), mean(x$PCovDT123), mean(x$PCovDB3), mean(x$PCovDB123), mean(x$PCovB3), mean(x$PCovB123))
  PCovs <- c(sum(x$PCovDT3*x$Pop), sum(x$PCovDT123*x$Pop), sum(x$PCovDB3*x$Pop), sum(x$PCovDB123*x$Pop), sum(x$PCovB3*x$Pop), sum(x$PCovB123*x$Pop))/sum(x$Pop)
  QAC <- rep(c("3", "123"), 3)
  Alg <- c(rep("DT", 2), rep("DB", 2), rep("B", 2))
  Out <- cbind.data.frame(Alg, QAC, Covs, PCovs)
  return(Out)
}

ddply(AllKinds, .(East2, Season), CalcRes2)

ddply(AllKinds, .(Season), CalcRes2)

toLatex.xtable(xtable(ddply(AllKinds, .(Landcover), CalcRes2)), include.rownames=F)

AllKinds$ElevCat <- cut(AllKinds$MeanElev, quantile(AllKinds$MeanElev))
ddply(AllKinds, .(ElevCat, East2), CalcRes2)

East <- read.csv("H:/Rotation_Yang/GridPopLakes2.csv")[,c(3,7:14)]
Spring3km <- read.csv("H:/Rotation_Yang/3kmGridding/AggDat3km3.csv")[,c(2,6:8)]
Summer3km <- read.csv("H:/Rotation_Yang/3kmGridding/AggDat3km6.csv")[,c(2,6:8)]
Fall3km <- read.csv("H:/Rotation_Yang/3kmGridding/AggDat3km9.csv")[,c(2,6:8)]
Winter3km <- read.csv("H:/Rotation_Yang/3kmGridding/AggDat3km12.csv")[,c(2,6:8)]
All3km <- read.csv("H:/Rotation_Yang/3kmGridding/AggDat3km.csv")[,c(2,6:8)]

AllKinds3km <- rbind.data.frame(All3km, Winter3km, Spring3km, Summer3km, Fall3km)
AllKinds3km$Season <- as.factor(c(rep("All", nrow(All3km)), rep("Winter", nrow(Winter3km)), rep("Spring", nrow(Spring3km)), rep("Summer", nrow(Summer3km)), rep("Fall", nrow(Fall3km))))
AllKinds3km2 <- merge(AllKinds3km, East, by.x="ID", by.y="Input_FID", all.x=T)
# Remove lake and ocean pixels
AllKinds3km <- AllKinds3km2[AllKinds3km2$Lake==0,]
# Create table
CalcRes <- function(x){
  Covs <- cbind.data.frame("DT", mean(x$PCovDT3), mean(x$PCovDT123), sum(x$PCovDT3*x$Pop)/sum(x$Pop), sum(x$PCovDT123*x$Pop)/sum(x$Pop))
  #PCovs <- c(sum(x$PCovDT3*x$Pop), sum(x$PCovDT123*x$Pop))/sum(x$Pop)
  #QAC <- c("3", "123")
  #Alg <- rep("DT", 2)
  #Out <- cbind.data.frame(Alg, QAC, Covs, PCovs)
  colnames(Covs) <- c("Algorithm", "DT3Coverage", "DT123Coverage", "DT3PopCov", "DT123PopCov")
  return(Covs)
}


toLatex.xtable(xtable(ddply(AllKinds3km, .(East2, Season), CalcRes)), include.rownames=F)

ddply(AllKinds3km, .(Season), CalcRes)

ddply(AllKinds3km, .(East2), CalcRes)

CalcRes(AllKinds3km)

toLatex.xtable(xtable(ddply(AllKinds3km, .(Landcover), CalcRes)), include.rownames=F)

