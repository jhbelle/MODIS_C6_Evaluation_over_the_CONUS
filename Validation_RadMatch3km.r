## --------------
## Name: Validation_RadMatch3km.r
## Program version: R 3.1.0
## Dependencies: modeest, plyr, ggplot2
## Function File: Functions_RadMatch3km.r
## Author: J.H. Belle
## Purpose: Perform analysis and export files for ground parameter matching. Perform correlation analyses
## --------------

## ---------
## Read in Aeronet data
## ---------

Aero04 <- read.csv("H:/Rotation_Yang/AERONETcollocs04_550Ints.csv")
Aero05 <- read.csv("H:/Rotation_Yang/AERONETcollocs05_550Ints.csv")
Aero06 <- read.csv("H:/Rotation_Yang/AERONETcollocs06_550Ints.csv")
Aero07 <- read.csv("H:/Rotation_Yang/AERONETcollocs07_550Ints.csv")
Aero08 <- read.csv("H:/Rotation_Yang/AERONETcollocs08_550Ints.csv")
Aero09 <- read.csv("H:/Rotation_Yang/AERONETcollocs09_550Ints.csv")
Aero10 <- read.csv("H:/Rotation_Yang/AERONETcollocs10_550Ints.csv")
Aero11 <- read.csv("H:/Rotation_Yang/AERONETcollocs11_550Ints.csv")
Aero12 <- read.csv("H:/Rotation_Yang/AERONETcollocs12_550Ints.csv")
Aero13 <- read.csv("H:/Rotation_Yang/AERONETcollocs13_550Ints.csv")

Aero1 <- rbind.data.frame(Aero04, Aero05, Aero06, Aero07, Aero08, Aero09, Aero10, Aero11, Aero12, Aero13)
Year <- c(rep(2004, nrow(Aero04)), rep(2005, nrow(Aero05)), rep(2006, nrow(Aero06)), rep(2007, nrow(Aero07)), rep(2008, nrow(Aero08)), rep(2009, nrow(Aero09)), rep(2010, nrow(Aero10)), rep(2011, nrow(Aero11)), rep(2012, nrow(Aero12)), rep(2013, nrow(Aero13)))
Aero <- na.omit(cbind.data.frame(Aero1, Year))
attributes(Aero)$na.action <- NULL
Aero$X <- NULL
Aero$Timestamp <- as.numeric(apply(Aero, 2, gsub, pattern=":", replacement="", x=Aero$Timestamp)[,1])
AOD550_exp <- exp(Aero$AOD550nm)
Aero <- cbind.data.frame(Aero, AOD550_exp)

rm(Aero04, Aero05, Aero06, Aero07, Aero08, Aero09, Aero10, Aero11, Aero12, Aero13, AOD550_exp, Aero1, Year)

## -------
## Read in MODIS data and merge with AERONET
## -------
library(modeest)
Filelist <- list.files("H:/Rotation_Yang/MOD3kmMatch", full.names=T)
source("H:/Rotation_Yang/Fuctions_RadMatch3km.r")

for (f in Filelist){
  Year <- as.integer(paste("20", substr(f, 47, 48), sep=""))
  MODdat <- read.csv(f)[,c(1:3,6:9)]
  MODdat <- subset(MODdat, MODdat$DistStation < 7500)
  Ntot <- aggregate(MODdat[,5], list(MODdat$AeroLoc, MODdat$JulianDate, MODdat$Passtime), length)
  QA3 <- try(merge(GetQAdat(MODdat, 3), Ntot, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3")))
  if (is.data.frame(QA3)) {
    QA3$Year <- rep(Year, nrow(QA3))
    QA3$PercAvail <- (QA3$NQA/QA3$x)*100
    DT3 <- subset(QA3, QA3$PercAvail >= 20)
    if (exists("Outp3")) {
      Outp3 <- rbind.data.frame(Outp3, DT3)
    } else Outp3=DT3
  }
  rm(DT3, QA3)
  QA0 <- try(merge(GetQAdat(MODdat, 0), Ntot, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3")))
  if (is.data.frame(QA0)) {
    QA0$Year <- rep(Year, nrow(QA0))
    QA0$PercAvail <- (QA0$NQA/QA0$x)*100
    DT0 <- subset(QA0, QA0$PercAvail >= 20)
    if (exists("Outp0")) {
      Outp0 <- rbind.data.frame(Outp0, DT0)
    } else Outp0=DT0
  }
  rm(DT0, QA0)
  QA1 <- try(merge(GetQAdat(MODdat, 1), Ntot, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3")))
  if (is.data.frame(QA1)) {
    QA1$Year <- rep(Year, nrow(QA1))
    QA1$PercAvail <- (QA1$NQA/QA1$x)*100
    DT1 <- subset(QA1, QA1$PercAvail >= 20)
    if (exists("Outp1")) {
      Outp1 <- rbind.data.frame(Outp1, DT1)
    } else Outp1=DT1
  }
  rm(DT1, QA1, Ntot, MODdat, Year)
}
DT33kM <- MergeAero(Aero, Outp3)
DT13kM <- MergeAero(Aero, Outp1)
DT03kM <- MergeAero(Aero, Outp0)

rm(Outp3, Outp0, Outp1, Aero, Filelist, f)

MODscale <- 0.0010000000474974513
## Scale AOD
DT33kM$AODMod <- DT33kM$AODMod*MODscale
DT13kM$AODMod <- DT13kM$AODMod*MODscale
DT03kM$AODMod <- DT03kM$AODMod*MODscale

cor(DT33kM$AODMod, DT33kM$AeroAOD)
cor(DT13kM$AODMod, DT13kM$AeroAOD)
cor(DT03kM$AODMod, DT03kM$AeroAOD)
## Prior to making extraction radius smaller, correlations were as follows: 0.8098461 (3); 0.8592477 (1); 0.8084383 (0)
## After switching to a 7.5 km radius, correlations were as follows: 0.7111507 (3); 0.8032195 (1); 0.800046 (0)

plot(DT03kM$AeroAOD~DT03kM$AODMod)
plot(DT13kM$AeroAOD~DT13kM$AODMod)
plot(DT33kM$AeroAOD~DT33kM$AODMod)

DT3Model <- lm(AODMod~AeroAOD, DT33kM)
dt3eeup = DT33kM$AeroAOD + (0.05+(0.15*DT33kM$AeroAOD))
dt3eed = DT33kM$AeroAOD - (0.05+(0.15*DT33kM$AeroAOD))
AboveEE = ifelse(DT33kM$AODMod > dt3eeup, 1, 0)
BelowEE = ifelse(DT33kM$AODMod < dt3eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)
summary(DT3Model)

library(aqfig)
library(RColorBrewer)
tiff("H:/Rotation_Yang/Imagery/DT3_3kmPic.tiff")
scatterplot.density(x=DT33kM$AeroAOD, y=DT33kM$AODMod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="3km Dark-Target (Very good observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eeup)
abline(a=0.05, b=1.15, lty=2, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eed)
abline(a=-0.05, b=0.85, lty=2, col=rgb(0,0,0,0.6))
#dt <- lm(AODavg ~MeanAOD, QA3DTM)
#summary(dt)
abline(DT3Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.92,y=0.22, "% within EE = 61.8 \n% above EE = 33.3 \n% below EE = 4.9 \nN = 11,867; r=0.71 \nY=0.03 + 1.24 (purple line)", cex=1.3)
dev.off()

DT1Model <- lm(AODMod~AeroAOD, DT13kM)
dt1eeup = DT13kM$AeroAOD + (0.05+(0.15*DT13kM$AeroAOD))
dt1eed = DT13kM$AeroAOD - (0.05+(0.15*DT13kM$AeroAOD))
AboveEE = ifelse(DT13kM$AODMod > dt1eeup, 1, 0)
BelowEE = ifelse(DT13kM$AODMod < dt1eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)
summary(DT1Model)

tiff("H:/Rotation_Yang/Imagery/DT1_3kmPic.tiff")
scatterplot.density(x=DT13kM$AeroAOD, y=DT13kM$AODMod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="3km Dark-Target (Marginal observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eeup)
abline(a=0.05, b=1.15, lty=2, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eed)
abline(a=-0.05, b=0.85, lty=2, col=rgb(0,0,0,0.6))
#dt <- lm(AODavg ~MeanAOD, QA3DTM)
#summary(dt)
abline(DT1Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.92,y=0.22, "% within EE = 85.6 \n% above EE = 11.2 \n% below EE = 3.2 \nN = 1,429; r=0.80 \nY=0.03 + 0.84 (purple line)", cex=1.3)
dev.off()

DT0Model <- lm(AODMod~AeroAOD, DT03kM)
dt0eeup = DT03kM$AeroAOD + (0.05+(0.15*DT03kM$AeroAOD))
dt0eed = DT03kM$AeroAOD - (0.05+(0.15*DT03kM$AeroAOD))
AboveEE = ifelse(DT03kM$AODMod > dt0eeup, 1, 0)
BelowEE = ifelse(DT03kM$AODMod < dt0eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)
summary(DT0Model)

tiff("H:/Rotation_Yang/Imagery/DT0_3kmPic.tiff")
scatterplot.density(x=DT03kM$AeroAOD, y=DT03kM$AODMod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="3km Dark-Target (No confidence observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eeup)
abline(a=0.05, b=1.15, lty=2, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eed)
abline(a=-0.05, b=0.85, lty=2, col=rgb(0,0,0,0.6))
#dt <- lm(AODavg ~MeanAOD, QA3DTM)
#summary(dt)
abline(DT0Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.92,y=0.22, "% within EE = 54.4 \n% above EE = 36.4 \n% below EE = 9.2 \nN = 3,703; r=0.80 \nY=0.02 + 1.28 (purple line)", cex=1.3)
dev.off()

library(plyr)
CorSeason(DT33kM)
CorSeason(DT13kM)
CorSeason(DT03kM)

CorMonth(DT33kM)
CorMonth(DT13kM)
CorMonth(DT03kM)


## --------------------
# Create files for matchup with ground parameters
## --------------------

for (f in Filelist){
  Year <- as.integer(paste("20", substr(f, 47, 48), sep=""))
  MODdat <- read.csv(f)[,c(1:3,6,9:17)]
  MODdat <- subset(MODdat, MODdat$DistStation < 7500)
  MODdat$Year <- rep(Year, nrow(MODdat))
  if (exists("MODstack")){
    MODstack <- rbind.data.frame(MODstack, MODdat)
  } else MODstack = MODdat
  rm(MODdat)
}

QA3DTLL <- subset(MODstack, MODstack$QAdt==3)[,c(1:3,6:14)]
QA3DT <- merge(QA3DTLL, DT33kM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA3DT, "H:/Rotation_Yang/ForLink3km_QA3DT.csv")

QA1DTLL <- subset(MODstack, MODstack$QAdt==1)[,c(1:3,6:14)]
QA1DT <- merge(QA1DTLL, DT13kM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA1DT, "H:/Rotation_Yang/ForLink3km_QA1DT.csv")

QA0DTLL <- subset(MODstack, MODstack$QAdt==0)[,c(1:3,6:14)]
QA0DT <- merge(QA0DTLL, DT03kM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA0DT, "H:/Rotation_Yang/ForLink3km_QA0DT.csv")

## -------------------
# Modeling following matchup - QA 3 and 1
## -------------------

DirDT3Match <- list.files("H://Rotation_Yang/MatchOver/DT33km", full.names=T)
for (file in DirDT3Match){
  fileR <- read.csv(file)[,2:6]
  DT33kM <- merge(DT33kM, fileR, by=c("AeroLoc", "JulianDate", "Passtime", "Year"))
}
colnames(DT33kM) <- c(colnames(DT33kM[1:15]), unlist(strsplit(list.files("H://Rotation_Yang/MatchOver/DT33km"), "[.]"))[c(1,3,5,7,9,11,13,15,17,19)])

#Histograms
for (i in c(2:6,9,11,12,15:25)){
  hist(DT33kM[,i], main=colnames(DT33kM)[i])
}


plot(DT33kM$Pop, DT33kM$PImpervMean)
plot(DT33kM$Pop, DT33kM$PImpervSum)
plot(DT33kM$Pop, DT33kM$NDVI)
plot(DT33kM$PImpervMean, DT33kM$PImpervSum)
plot(DT33kM$PWetNLCD, DT33kM$PImpervSum)
plot(DT33kM$PWetNLCD, DT33kM$PImpervMean)
plot(DT33kM$PWetNLCD, DT33kM$Pop)
plot(DT33kM$PWetNLCD, DT33kM$NLCDMode)
plot(DT33kM$PWetNLCD, DT33kM$NDVI)
plot(DT33kM$NDVI, DT33kM$PImpervSum)
plot(DT33kM$NDVI, DT33kM$PImpervMean)
plot(DT33kM$NDVI, DT33kM$Watercm)
# Cut passtime into chunks based on separate passes around globe
DT33kM$Passtime2 <- cut(DT33kM$Passtime/5, breaks=c(300, 332, 352, 372, 392, 412, 432, 452), labels=c(1,2,3,4,5,6,7))
plot(AODMod~Passtime2, DT33kM)
cor(DT33kM[,c(2:6,9,11,12,15:25)], use="complete.obs")

Mod1 <- lm(DT33kM$AODMod~DT33kM$AeroAOD)
summary(Mod1)
plot(DT33kM$AODMod~DT33kM$AeroAOD, ylim=c(0,2.5), xlim=c(0,2.5))
abline(a=0, b=1)
Mod2 <- lm(AODMod~ AeroAOD + PImpervMean + Watercm + Watercm*PImpervMean, DT33kM)
summary(Mod2)
plot(subset(DT33kM$AODMod, !is.na(DT33kM$PImpervMean))~predict(Mod2), ylab="Original MODIS AOD", xlab="Model Predicted AOD", ylim=c(0,2.5), xlim=c(0,2.5))
abline(a=0, b=1, col=rgb(0,0,0,0.6))
Mod3 <- lm(AODMod~AeroAOD + PImpervMean + Watercm + Watercm*PImpervMean + NDVI + MadSlope + Passtime + AeroAOD*NDVI + AeroAOD*MadSlope + NDVI*MadSlope + Passtime*MadSlope, DT33kM)
summary(Mod3)
plot(subset(DT33kM$AODMod, !is.na(DT33kM$PImpervMean) & !is.na(DT33kM$NDVI))~predict(Mod3), ylab="Original MODIS AOD", xlab="Model Predicted AOD", ylim=c(0,2.5), xlim=c(0,2.5))
abline(a=0, b=1, col=rgb(0,0,0,0.6))

# Look into why passtime is significant - stratified models on pass number (pass over globe) - reflects East/West differences
for (i in seq(1, 7)){
  PassSub <- subset(DT33kM, DT33kM$Passtime2 == i)
  plot(PassSub$AODMod ~ PassSub$AeroAOD, ylim=c(0,2.5), xlim=c(0,2.5), main=paste("Passblock", i))
  abline(a=0, b=1)
  Mod <- lm(PassSub$AODMod ~ PassSub$AeroAOD)
  print(summary(Mod))
}
# Observed similiar patterns of association for blocks 1-4, and 5-7
DT33kM$PasstimeBreak <- cut(as.numeric(DT33kM$Passtime2), breaks=c(0,3.5,5.5,8), labels=c("E", "M", "W"))
ggplot(DT33kM, aes(x=AeroAOD, y=AODMod)) + geom_point() + ylim(c(0,2.5)) + xlim(c(0,2.5)) + facet_grid(PasstimeBreak~.) + theme_classic()

# Look into why passtime is significant - stratified models on passtime while ignoring pass number - looking into geometry differences (N/S)
DT33kM$Passtime3 <- DT33kM$Passtime/5-(300+20*as.numeric(DT33kM$Passtime2))
for (i in seq(1, 12)){
  PassSub <- subset(DT33kM, DT33kM$Passtime3 == i-1)
  plot(PassSub$AODMod ~ PassSub$AeroAOD, ylim=c(0,2.5), xlim=c(0,2.5), main=paste("Relative Granule Location", i-1))
  abline(a=0, b=1)
  Mod <- lm(PassSub$AODMod ~ PassSub$AeroAOD)
  print(summary(Mod))
}

SubPass2 <- subset(DT33kM, DT33kM$PasstimeBreak == 3)
for (i in seq(1, 12)){
  GSub <- subset(SubPass2, SubPass2$Passtime3 == i-1)
  plot(GSub$AODMod~GSub$AeroAOD, ylim=c(0,2.5), xlim=c(0,2.5), main=paste("Relative Granule Location", i-1))
  abline(a=0, b=1)
  print(summary(lm(GSub$AODMod~GSub$AeroAOD)))
}

# Check how well Passtime divisions line up with E/W division using traditional dividing line 
East_locs <- read.csv("H:/Rotation_Yang/EastWestSplit/EastLocs.csv")[,2]
East <- rep(1, length(East_locs))
East <- cbind.data.frame(East_locs, East)
DT33kM2 <- merge(DT33kM, East, by.x="AeroLoc", by.y="East_locs", all.x=T)
DT33kM2$East <- ifelse(is.na(DT33kM2$East), 0,1)
xtabs(~PasstimeBreak + East, DT33kM2)
ggplot(DT33kM2, aes(x=AeroAOD, y=AODMod)) + geom_point() + ylim(c(0,2.5)) + xlim(c(0,2.5)) + facet_grid(East~.) + theme_classic()
summary(lm(AODMod~AeroAOD, subset(DT33kM2, DT33kM2$PasstimeBreak=="E")))
summary(lm(AODMod~AeroAOD, subset(DT33kM2, DT33kM2$East==1)))

summary(lm(AODMod~AeroAOD, subset(DT33kM2, DT33kM2$PasstimeBreak!="E")))
summary(lm(AODMod~AeroAOD, subset(DT33kM2, DT33kM2$East!=1)))

# Look further into passtime divisions - Identify way to break country up into 3 sections instead of 2, check, within each section for the effect of watercm and NDVI - may need to look w/in strata
DT33kM2$East <- cut(DT33kM2$East, breaks=c(-0.5, 0.5, 1.5), labels=c("West", "East"))
ggplot(DT33kM2, aes(x=AeroAOD, y=AODMod, col=NDVI)) + geom_abline(intercept=0, slope=1, col="gray80") + geom_point(alpha=0.5) + facet_grid(East~.) + scale_x_continuous("Aeronet AOD", limits=c(0,2.5)) + scale_y_continuous("MODIS AOD", limits=c(0,2.5)) + scale_color_gradient(low="purple", high="green") + theme_classic() 
ggplot(DT33kM2, aes(x=AeroAOD, y=AODMod, col=NDVI)) + geom_point() + facet_grid(PasstimeBreak~.) + theme_classic()
ggplot(DT33kM2, aes(x=AeroAOD, y=AODMod, col=Watercm)) + geom_point() + facet_grid(East~.) + theme_classic() 
ggplot(DT33kM2, aes(x=AeroAOD, y=AODMod, col=Watercm)) + geom_point(alpha=0.5) + facet_grid(PasstimeBreak~.) + theme_classic()

ggplot(DT33kM2, aes(x=AeroAOD, y=AODMod, z=NDVI)) + 
  geom_abline(intercept=0, slope=1, col="gray80", linetype="dashed") + 
  stat_summary2d(bins=100, fun=median) + 
  facet_grid(East~.) + 
  #scale_fill_gradient2("NDVI", low=muted("purple"), high=muted("green"), mid="gray90", midpoint=0.42, limits=c(0,1)) +
  scale_fill_gradientn("NDVI", colours=c("black", muted("purple"), "gray90", muted("green")), values=c(0, 0.25, 0.45, 1)) +
  scale_x_continuous("Aeronet AOD", limits=c(0,2)) + 
  scale_y_continuous("MODIS AOD", limits=c(0,2)) + 
  theme_classic() 

DT33kM2$NDVIp1cuts <- cut(DT33kM2$NDVI, breaks=seq(-0.2, 1, 0.1), labels=seq(1,12))
DT33kM2$NDVIdeciles <- cut(DT33kM2$NDVI, breaks= quantile(DT33kM2$NDVI, probs=seq(0,1,0.1), na.rm=T, names=F), labels=seq(1,10), include.lowest=T)

Modp1cuts_w <- lm(AODMod~AeroAOD + NDVIp1cuts + AeroAOD*NDVIp1cuts, subset(DT33kM2, DT33kM2$East=="West"))
summary(Modp1cuts_w)
b <- Modp1cuts_w$coefficients
NDVIbaod1_cuts <- c(0, b[3] + b[13], b[4] + b[14], b[5] + b[15], b[6] + b[16], b[7] + b[17], b[8] + b[18], b[9] + b[19], b[10] + b[20], b[11] + b[21], b[12])
plot(NDVIbaod1_cuts)
Modp1cuts_e <- lm(AODMod~AeroAOD + NDVIp1cuts + AeroAOD*NDVIp1cuts, subset(DT33kM2, DT33kM2$East=="East"))
summary(Modp1cuts_e)
b <- Modp1cuts_e$coefficients
NDVIbaod1_cuts <- c(0, b[3] + b[13], b[4] + b[14], b[5] + b[15], b[6] + b[16], b[7] + b[17], b[8] + b[18], b[9] + b[19], b[10] + b[20], b[11] + b[21], b[12]+b[22])
plot(NDVIbaod1_cuts)

Moddeciles_w <- lm(AODMod~AeroAOD + NDVIdeciles + AeroAOD*NDVIdeciles, subset(DT33kM2, DT33kM2$East=="West"))
summary(Moddeciles_w)
b <- Moddeciles_w$coefficients
NDVIdeciles_cuts <- c(0, b[3] + b[12], b[4] + b[13], b[5] + b[14], b[6] + b[15], b[7] + b[16], b[8] + b[17], b[9] + b[18], b[10] + b[19], b[11] + b[20])
plot(NDVIdeciles_cuts)
Moddeciles_e <- lm(AODMod~AeroAOD + NDVIdeciles + AeroAOD*NDVIdeciles, subset(DT33kM2, DT33kM2$East=="East"))
summary(Moddeciles_e)
b <- Moddeciles_e$coefficients
NDVIdeciles_cuts <- c(0, b[3] + b[12], b[4] + b[13], b[5] + b[14], b[6] + b[15], b[7] + b[16], b[8] + b[17], b[9] + b[18], b[10] + b[19], b[11] + b[20])
plot(NDVIdeciles_cuts)

SubPass2 <- subset(DT33kM2, DT33kM2$East == "West")
#SubPass2 <- subset(DT33kM2, DT33kM2$PasstimeBreak == "W")
for (i in seq(1, 12)){
  GSub <- subset(SubPass2, SubPass2$NDVIp1cuts == as.character(i))
  plot(GSub$AODMod~GSub$AeroAOD, ylim=c(0,2.5), xlim=c(0,2.5), main=paste("Part", i))
  abline(a=0, b=1)
  print(summary(lm(GSub$AODMod~GSub$AeroAOD)))
}
for (i in seq(1, 10)){
  GSub <- subset(SubPass2, SubPass2$NDVIdeciles == as.character(i))
  plot(GSub$AODMod~GSub$AeroAOD, ylim=c(0,2.5), xlim=c(0,2.5), main=paste("Decile", i))
  abline(a=0, b=1)
  print(summary(lm(GSub$AODMod~GSub$AeroAOD)))
}

DT33kM2$NDVIcats <- cut(DT33kM2$NDVI, c(-0.3, 0.25, 0.45, 1), labels=c("L", "M", "H"))
DT33kM2$NDVIcats2 <- cut(DT33kM2$NDVI, c(-0.3, 0.45, 1), labels=c("L", "H"))
DT33kM2$Scrub <- ifelse(DT33kM2$NLCDMode == 52, 1, 0)
DT33kM2$Developed <- cut(DT33kM2$NLCDMode, c(-1, 21.5, 22.5, 24.5, 100), labels=c("NA1", "L", "MH", "NA2"))
DT33kM2$Developed <- ifelse(DT33kM2$Developed == "NA1" | DT33kM2$Developed == "NA2", 1, as.factor(DT33kM2$Developed))
DT33kM2$NDVIcats <- relevel(DT33kM2$NDVIcats, ref="H")

Basee <-lm(AODMod~AeroAOD, subset(DT33kM2, DT33kM2$East == "East"))
summary(Basee)
M1e <- lm(AODMod~AeroAOD + NDVIcats2 + AeroAOD*NDVIcats2 + I(AeroAOD^2), subset(DT33kM2, DT33kM2$East=="East"))
summary(M1e)
M2e <- lm(AODMod~(AeroAOD + PImpervMean + PWetNLCD + Watercm + MedSlope)^2 - AeroAOD:PWetNLCD, subset(DT33kM2, DT33kM2$East=="East"))
summary(M2e)
M3e <- lm(AODMod~(AeroAOD + Watercm + PImpervMean + NDVIcats + MedSlope + PWetNLCD + as.factor(Developed))^2, subset(DT33kM2, DT33kM2$East=="East"))
summary(M3e)

Basew <- lm(AODMod~AeroAOD, subset(DT33kM2, DT33kM2$East == "West"))
summary(Basew)
M1w <- lm(AODMod~AeroAOD + NDVIcats + AeroAOD*NDVIcats, subset(DT33kM2, DT33kM2$East=="West"))
summary(M1w)
M2w <- lm(AODMod~(AeroAOD + NDVIcats + PImpervMean)^2, subset(DT33kM2, DT33kM2$East=="West"))
summary(M2w)
M3w <- lm(AODMod~(AeroAOD + PImpervMean + NDVIcats + Watercm + Scrub + MedSlope)^2, subset(DT33kM2, DT33kM2$East=="West"))
summary(M3w)
M4w <- lm(AODMod~(AeroAOD + NDVIcats + Watercm + PImpervMean + MedSlope + PWetNLCD + as.factor(Developed) + Scrub)^2 - as.factor(Developed)*Scrub, subset(DT33kM2, DT33kM2$East=="West"))
summary(M4w)

# Look into categorizing PImpervMean and Watercm
DT33kM2$PIDeciles <- cut(DT33kM2$PImpervMean, breaks= quantile(DT33kM2$PImpervMean, probs=seq(0,1,0.1), na.rm=T, names=F), labels=seq(1,10), include.lowest=T)
DT33kM2$WDeciles <- cut(DT33kM2$Watercm, breaks=quantile(DT33kM2$Watercm, probs=seq(0,1,0.1), na.rm=T, names=F), labels=seq(1,10), include.lowest=T)


## ----------------
# Extreme values analysis
## ----------------

Extreme_thresh3km <- rbind.data.frame(ExVals(DT33kM$AeroAOD, DT33kM$AODMod), ExVals(DT13kM$AeroAOD, DT13kM$AODMod), ExVals(DT03kM$AeroAOD, DT03kM$AODMod))
Extreme_thresh3km$Alg <- rep("DT - 3km", 3)
Extreme_thresh3km$QAC <- c(3, 1, 0)
Extreme_thresh3km
Extreme_thresh <- rbind.data.frame(Extreme_thresh, Extreme_thresh3km)
format(Extreme_thresh[,c(5:7,1:4)], digits=3)

East_locs <- read.csv("H:/Rotation_Yang/EastWestSplit/EastLocs100Long.csv")[,3]
East <- rep(1, length(East_locs))
East <- cbind.data.frame(East_locs, East)
DT33kM <- AddEW(East, DT33kM)
DT13kM  <- AddEW(East, DT13kM)
DT03kM  <- AddEW(East, DT03kM)
Extreme <- rbind.data.frame(ExVals(DT33kM$AeroAOD, DT33kM$AODMod, thresh=c(quantile(DT33kM$AeroAOD[DT33kM$East=="West"], probs=0.95), quantile(DT33kM$AeroAOD[DT33kM$East=="East"], probs=0.95)), subseter=DT33kM$East), ExVals(DT13kM$AeroAOD, DT13kM$AODMod, thresh=c(quantile(DT13kM$AeroAOD[DT13kM$East=="West"], probs=0.95), quantile(DT13kM$AeroAOD[DT13kM$East=="East"], probs=0.95)), subseter=DT13kM$East))
Extreme$Alg <- c(rep("DT - 3km", 4))
Extreme$QAC <- rep(c(3,3,1,1), 1)
format(Extreme, digits=3)


Extreme0p5 <- rbind.data.frame(ExVals(DT33kM$AeroAOD, DT33kM$AODMod, thresh=c(0.5, 0.5), subseter=DT33kM$East), ExVals(DT13kM$AeroAOD, DT13kM$AODMod, thresh=c(0.5, 0.5), subseter=DT13kM$East))
Extreme0p5$Alg <- c(rep("DT - 3km", 4))
Extreme0p5$QAC <- rep(c(3,3,1,1), 1)
format(Extreme0p5, digits=3)


## ------------------
# Accuracy analysis - final version
## ------------------
