## --------------
## Name: Validation_RadMatch10km.r
## Program version: R 3.1.0
## Dependencies: modeest
## Function File: Functions_RadMatch10km.r
## Author: J.H. Belle
## Purpose: Perform and eport files for radial matching. Perform correlation analyses, match to ground parameters
## --------------

library(modeest)
library(plyr)

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

Modis04 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2004.csv")[,1:13]
Modis05 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2005.csv")[,1:13]
Modis06 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2006.csv")[,1:13]
Modis07 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2007.csv")[,1:13]
Modis08 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2008.csv")[,1:13]
Modis09 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2009.csv")[,1:13]
Modis10 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2010.csv")[,1:13]
Modis11 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2011.csv")[,1:13]
Modis12 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2012.csv")[,1:13]
Modis13 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2013.csv")[,1:13]
source("H:/Rotation_Yang/Fuctions_RadMatch10km.r")
QA3DT <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 3, c(1:3,7,10,11))
QA3DTM <- MergeAero(Aero, QA3DT)
write.csv(QA3DTM, "H:/Rotation_Yang/MergedModisAero_DT3.csv")
rm(QA3DT)
QA2DT <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 2, c(1:3,7,10,11))
QA2DTM <- MergeAero(Aero, QA2DT)
rm(QA2DT)
write.csv(QA2DTM, "H:/Rotation_Yang/MergedModisAero_DT2.csv")
QA1DT <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 1, c(1:3,7,10,11))
QA1DTM <- MergeAero(Aero, QA1DT)
rm(QA1DT)
write.csv(QA1DTM, "H:/Rotation_Yang/MergedModisAero_DT1.csv")
#QA0DT <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 0, c(1:3,7,10,11))
#QA0DTM <- MergeAero(Aero, QA0DT)
#rm(QA0DT)
QA3DB <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 3, c(1:3,8,10,12))
QA3DBM <- MergeAero(Aero, QA3DB)
rm(QA3DB)
write.csv(QA3DBM, "H:/Rotation_Yang/MergedModisAero_DB3.csv")
QA2DB <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 2, c(1:3,8,10,12))
QA2DBM <- MergeAero(Aero, QA2DB)
rm(QA2DB)
write.csv(QA2DBM, "H:/Rotation_Yang/MergedModisAero_DB2.csv")
QA1DB <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 1, c(1:3,8,10,12))
QA1DBM <- MergeAero(Aero, QA1DB)
rm(QA1DB)
write.csv(QA1DBM, "H:/Rotation_Yang/MergedModisAero_DB1.csv")
QA3B <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 3, c(1:3,9,10,13))
QA3BM <- MergeAero(Aero, QA3B)
rm(QA3B)
write.csv(QA3BM, "H:/Rotation_Yang/MergedModisAero_B3.csv")
QA2B <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 2, c(1:3,9,10,13))
QA2BM <- MergeAero(Aero, QA2B)
rm(QA2B)
write.csv(QA2BM, "H:/Rotation_Yang/MergedModisAero_B2.csv")
QA1B <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 1, c(1:3,9,10,13))
QA1BM <- MergeAero(Aero, QA1B)
rm(QA1B)
write.csv(QA1BM, "H:/Rotation_Yang/MergedModisAero_B1.csv")

#Modis <- rbind.data.frame(Modis11, Modis12, Modis13)
#Modis$Year <- c(rep(2011, length(Modis11[,1])), rep(2012, length(Modis12[,1])), rep(2013, length(Modis13[,1])))
rm(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, Aero)

## ---------
## Run models on existing data and save results
## ---------
library("car")
MODscale <- 0.0010000000474974513
## Scale AOD
QA1BM$ModAod <- QA1BM$ModAod*MODscale
QA1DBM$ModAod <- QA1DBM$ModAod*MODscale
QA1DTM$ModAod <- QA1DTM$ModAod*MODscale
QA2BM$ModAod <- QA2BM$ModAod*MODscale
QA2DBM$ModAod <- QA2DBM$ModAod*MODscale
QA2DTM$ModAod <- QA2DTM$ModAod*MODscale
QA3BM$ModAod <- QA3BM$ModAod*MODscale
QA3DBM$ModAod <- QA3DBM$ModAod*MODscale
QA3DTM$ModAod <- QA3DTM$ModAod*MODscale

DT3Model <- lm(ModAod~MeanAOD, QA3DTM)
outlierTest(DT3Model)
qqPlot(DT3Model, main="QQ Plot")
leveragePlots(DT3Model)
cutoff <- 4/((nrow(QA3DTM)-length(DT3Model$coefficients)-2))
plot(DT3Model, which=4, cook.levels=cutoff)
influencePlot(DT3Model, id.method="identify", main="Influence Plot", sub="Circle size proportional to Cook's Distance")
# Removed points 4192, 4194, and 6056 - outliers with influence
which(rownames(QA3DTM)==4192)
which(rownames(QA3DTM)==4194)
which(rownames(QA3DTM)==6056)
DT3Mod_NOutliers <- lm(ModAod~MeanAOD, QA3DTM[c(1:3662,3665:5330,5332:5525),])
plot(DT3Mod_NOutliers, which=4, cook.levels=cutoff)
summary(DT3Mod_NOutliers)
influencePlot(DT3Mod_NOutliers, id.method="identify")
plot(ModAod~MeanAOD, QA3DTM[c(1:3662,3665:5330,5332:5525),])
abline(a=0, b=1, col="red")
abline(DT3Mod_NOutliers, col="blue")

QA3DBM$ModAod <- QA3DBM$ModAod*0.0010000000474974513
DB3Model <- lm(ModAod~MeanAOD, QA3DBM)
outlierTest(DB3Model)
qqPlot(DB3Model, main="QQ Plot")
leveragePlots(DB3Model)
cutoff <- 4/((nrow(QA3DBM)-length(DB3Model$coefficients)-2))
plot(DB3Model, which=4, cook.levels=cutoff)
influencePlot(DB3Model, id.method="identify", main="Influence Plot", sub="Circle size proportional to Cook's Distance")
# Remove points w/ row names 4528, 4532, 4932
which(rownames(QA3DBM)==307)
which(rownames(QA3DBM)==687)
which(rownames(QA3DBM)==4528)
which(rownames(QA3DBM)==4932)
which(rownames(QA3DBM)==4532)
DB3Mod_NOutliers <- lm(ModAod~MeanAOD, QA3DBM[c(1:4061,4063,4065:4423,4425:6573),])
plot(DB3Mod_NOutliers, which=4, cook.levels=cutoff)
summary(DB3Mod_NOutliers)
influencePlot(DB3Mod_NOutliers, id.method="identify")
leveragePlots(DB3Mod_NOutliers)
plot(ModAod~MeanAOD, QA3DBM[c(1:255,257:586,588:4061,4063:4395,4397:4423,4425:6573),])
abline(a=0, b=1, col="red")
abline(DB3Mod_NOutliers, col="blue")

#QA3BM$ModAod <- QA3BM$ModAod*0.0010000000474974513
B3Model <- lm(ModAod~MeanAOD, QA3BM)
outlierTest(B3Model)
qqPlot(B3Model, main="QQ Plot")
leveragePlots(B3Model)
cutoff <- 4/((nrow(QA3BM)-length(B3Model$coefficients)-2))
plot(B3Model, which=4, cook.levels=cutoff)
influencePlot(B3Model, id.method="identify", main="Influence Plot", sub="Circle size proportional to Cook's Distance")
# Remove outliers - 5108, 7358, 5111, 752
which(rownames(QA3BM)==752)
which(rownames(QA3BM)==5108)
which(rownames(QA3BM)==5111)
which(rownames(QA3BM)==7358)
B3Mod_NOutliers <- lm(MeanAOD~ModAod, QA3BM[c(1:641,643:4501,4503,4505:6522,6524:6917),])
plot(B3Mod_NOutliers, which=4, cook.levels=cutoff)
summary(B3Mod_NOutliers)
influencePlot(B3Mod_NOutliers, id.method="identify")
leveragePlots(B3Mod_NOutliers)
plot(MeanAOD~ModAod, QA3BM, xlab="MODIS AOD", ylab="AERONET AOD", col="gray75") #[c(1:641,643:4501,4503,4505:6522,6524:6917),])
points(MeanAOD~ModAod, QA3BM[c(1:641,643:4501,4503,4505:6522,6524:6917),], add=T)
abline(a=0, b=1, col="red")
abline(B3Model, col="blue")
abline(B3Mod_NOutliers, col="lightblue")
SeqVals = seq(0, 2, 0.01)
EEUp = (SeqVals*0.15 + 0.05)+SeqVals
EED = SeqVals - (SeqVals*0.05) -0.05
lines(SeqVals~EEUp)
lines(SeqVals~EED)

# Calculate the percentage of observations within the EE +/-(0.05+0.15MeanAOD)
dt3eeup = QA3DTM$MeanAOD + (0.05+(0.15*QA3DTM$MeanAOD))
dt3eed = QA3DTM$MeanAOD - (0.05+(0.15*QA3DTM$MeanAOD))
AboveEE = ifelse(QA3DTM$ModAod > dt3eeup, 1, 0)
BelowEE = ifelse(QA3DTM$ModAod < dt3eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)

db3eeup = QA3DBM$MeanAOD + (0.05+(0.15*QA3DBM$MeanAOD))
db3eed = QA3DBM$MeanAOD - (0.05+(0.15*QA3DBM$MeanAOD))
AboveEE = ifelse(QA3DBM$ModAod > db3eeup, 1, 0)
BelowEE = ifelse(QA3DBM$ModAod < db3eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)

b3eeup = QA3BM$MeanAOD + (0.05+(0.15*QA3BM$MeanAOD))
b3eed = QA3BM$MeanAOD - (0.05+(0.15*QA3BM$MeanAOD))
AboveEE = ifelse(QA3BM$ModAod > b3eeup, 1, 0)
BelowEE = ifelse(QA3BM$ModAod < b3eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)

dt2eeup = QA2DTM$MeanAOD + (0.05+(0.15*QA2DTM$MeanAOD))
dt2eed = QA2DTM$MeanAOD - (0.05+(0.15*QA2DTM$MeanAOD))
AboveEE = ifelse(QA2DTM$ModAod > dt2eeup, 1, 0)
BelowEE = ifelse(QA2DTM$ModAod < dt2eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)

db2eeup = QA2DBM$MeanAOD + (0.05+(0.15*QA2DBM$MeanAOD))
db2eed = QA2DBM$MeanAOD - (0.05+(0.15*QA2DBM$MeanAOD))
AboveEE = ifelse(QA2DBM$ModAod > db2eeup, 1, 0)
BelowEE = ifelse(QA2DBM$ModAod < db2eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)

b2eeup = QA2BM$MeanAOD + (0.05+(0.15*QA2BM$MeanAOD))
b2eed = QA2BM$MeanAOD - (0.05+(0.15*QA2BM$MeanAOD))
AboveEE = ifelse(QA2BM$ModAod > b2eeup, 1, 0)
BelowEE = ifelse(QA2BM$ModAod < b2eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)

dt1eeup = QA1DTM$MeanAOD + (0.05+(0.15*QA1DTM$MeanAOD))
dt1eed = QA1DTM$MeanAOD - (0.05+(0.15*QA1DTM$MeanAOD))
AboveEE = ifelse(QA1DTM$ModAod > dt1eeup, 1, 0)
BelowEE = ifelse(QA1DTM$ModAod < dt1eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)

db1eeup = QA1DBM$MeanAOD + (0.05+(0.15*QA1DBM$MeanAOD))
db1eed = QA1DBM$MeanAOD - (0.05+(0.15*QA1DBM$MeanAOD))
AboveEE = ifelse(QA1DBM$ModAod > db1eeup, 1, 0)
BelowEE = ifelse(QA1DBM$ModAod < db1eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)

b1eeup = QA1BM$MeanAOD + (0.05+(0.15*QA1BM$MeanAOD))
b1eed = QA1BM$MeanAOD - (0.05+(0.15*QA1BM$MeanAOD))
AboveEE = ifelse(QA1BM$ModAod > b1eeup, 1, 0)
BelowEE = ifelse(QA1BM$ModAod < b1eed, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)
# Models above are backwards from literature - switching and re-reporting
#plot(ModAod~MeanAOD, QA3DTM, col=rgb(0,0,0,.2), ylab="MODIS 550nm AOD", xlab="AERONET 550nm AOD", main="10km Dark-target", ylim=c(0,1.2), xlim=c(0,1.2))
#abline(a=0, b=1)
#line(QA3DTM$MeanAOD, dt3eeup)
#abline(a=0.05, b=1.15, lty=2)
#line(QA3DTM$MeanAOD, dt3eed)
#abline(a=-0.05, b=0.85, lty=2)
#dt <- lm(ModAod ~MeanAOD, QA3DTM)
#summary(dt)
#abline(DT3Model, col="firebrick1")
#text(x=1,y=0.2, "% within EE = 71.5 \n% above EE = 20.6 \n% below EE = 7.9 \nY=0.01 + 1.09")

#plot(ModAod~MeanAOD,QA3DBM, col="gray50", ylab="MODIS 550nm AOD", xlab="AERONET 550nm AOD")
#abline(a=0, b=1)
#lines(QA3DBM$MeanAOD, db3eeup)
#lines(QA3DBM$MeanAOD, db3eed)
#db <- lm(ModAod~MeanAOD, QA3DBM)
#summary(db)
#abline(db, col="red")

#plot(ModAod~MeanAOD,QA3BM, col="gray50")
#abline(a=0, b=1)
#lines(QA3BM$MeanAOD, b3eeu, lty=2)
#lines(QA3BM$MeanAOD, b3eed, tly=2)
#b <- lm(ModAod~MeanAOD, QA3BM)
#summary(b)
#abline(b, col="red")

# Run models
B3Model <- lm(ModAod~MeanAOD, QA3BM)
DB3Model <- lm(ModAod~MeanAOD, QA3DBM)
DT3Model <- lm(ModAod~MeanAOD, QA3DTM)
B2Model <- lm(ModAod~MeanAOD, QA2BM)
DB2Model <- lm(ModAod~MeanAOD, QA2DBM)
DT2Model <- lm(ModAod~MeanAOD, QA2DTM)
B1Model <- lm(ModAod~MeanAOD, QA1BM)
DB1Model <- lm(ModAod~MeanAOD, QA1DBM)
DT1Model <- lm(ModAod~MeanAOD, QA1DTM)
summary(B3Model)
summary(DB3Model)
summary(DT3Model)
summary(B2Model)
summary(DB2Model)
summary(DT2Model)
summary(B1Model)
summary(DB1Model)
summary(DT1Model)

# Create Plots for abstract
#PlotDT <- cbind.data.frame(QA3DTM$MeanAOD, dt3eeup, dt3eed)
#ggplot(QA3DTM, aes(y=ModAod, x=MeanAOD)) + geom_point(alpha=0.3, color="black") + geom_abline(a=0, b=1, size=1, color="gray", alpha=0.8) + geom_abline(a=0.05, b=1.15) + geom_abline(a=-0.05, b=0.85) + geom_smooth(method="lm", fill="gray90", color="red", alpha=0.5) + xlab("AERONET 550 nm AOD") + ylab("MODIS 550 nm AOD") + theme_classic()

library(aqfig)
library(RColorBrewer)
tiff("H:/Rotation_Yang/Imagery/DT3Pic.tiff")
scatterplot.density(x=QA3DTM$MeanAOD, y=QA3DTM$ModAod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="10km Dark-Target (Very good observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eeup)
abline(a=0.05, b=1.15, lty=2, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eed)
abline(a=-0.05, b=0.85, lty=2, col=rgb(0,0,0,0.6))
#dt <- lm(ModAod ~MeanAOD, QA3DTM)
#summary(dt)
abline(DT3Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.92,y=0.22, "% within EE = 74.4 \n% above EE = 18.7 \n% below EE = 6.8 \nN = 14,223; r=0.83 \nY=0.01 + 1.09 (purple line)", cex=1.3)
dev.off()

tiff("H:/Rotation_Yang/Imagery/DB3Pic.tiff")
scatterplot.density(x=QA3DBM$MeanAOD, y=QA3DBM$ModAod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="10km Deep-Blue (Very good observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1)
#line(QA3DBM$MeanAOD, db3eeup)
abline(a=0.04995, b=1.15071, lty=2)
#line(QA3DBM$MeanAOD, db3eed)
abline(a=-0.05004, b=0.85052, lty=2)
#db_no_int <- lm(ModAod ~MeanAOD+0, QA3DBM)
#summary(db_no_int)
abline(DB3Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.92,y=0.22, "% within EE = 84.4 \n% above EE = 11.0 \n% below EE = 4.6 \nN = 17,279; r=0.71 \nY=0.02 + 0.79 (purple line)", cex=1.3)
dev.off()

tiff("H:/Rotation_Yang/Imagery/B3Pic.tiff")
scatterplot.density(x=QA3BM$MeanAOD, y=QA3BM$ModAod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="10km Merged (Very good observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1)
#line(QA3BM$MeanAOD, b3eeup)
abline(a=0.05, b=1.15, lty=2)
#line(QA3BM$MeanAOD, b3eed)
abline(a=-0.05, b=0.85, lty=2)
#dt <- lm(ModAod ~MeanAOD, QA3DTM)
#summary(dt)
abline(B3Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.9,y=0.22, "% within EE = 78.8 \n% above EE = 15.3 \n% below EE = 6.0 \nN = 18,340; r=0.83 \nY=0.01 + 1.09 (purple line)", cex=1.3)
dev.off()

tiff("H:/Rotation_Yang/Imagery/DT2Pic.tiff")
scatterplot.density(x=QA2DTM$MeanAOD, y=QA2DTM$ModAod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="10km Dark-Target (Good observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eeup)
abline(a=0.05, b=1.15, lty=2, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eed)
abline(a=-0.05, b=0.85, lty=2, col=rgb(0,0,0,0.6))
#dt <- lm(ModAod ~MeanAOD, QA3DTM)
#summary(dt)
abline(DT2Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.92,y=0.22, "% within EE = 46.0 \n% above EE = 50.2 \n% below EE = 3.8 \nN = 4,115; r=0.69 \nY=0.06 + 1.21 (purple line)", cex=1.3)
dev.off()

tiff("H:/Rotation_Yang/Imagery/DB2Pic.tiff")
scatterplot.density(x=QA2DBM$MeanAOD, y=QA2DBM$ModAod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="10km Deep-Blue (Good observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1)
#line(QA3DBM$MeanAOD, db3eeup)
abline(a=0.04995, b=1.15071, lty=2)
#line(QA3DBM$MeanAOD, db3eed)
abline(a=-0.05004, b=0.85052, lty=2)
#db_no_int <- lm(ModAod ~MeanAOD+0, QA3DBM)
#summary(db_no_int)
abline(DB2Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.92,y=0.22, "% within EE = 67.6 \n% above EE = 29.3 \n% below EE = 3.1 \nN = 2,684; r=0.65 \nY=0.04 + 0.88 (purple line)", cex=1.3)
dev.off()

tiff("H:/Rotation_Yang/Imagery/B2Pic.tiff")
scatterplot.density(x=QA2BM$MeanAOD, y=QA2BM$ModAod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="10km Merged (Good observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1)
#line(QA3BM$MeanAOD, b3eeup)
abline(a=0.05, b=1.15, lty=2)
#line(QA3BM$MeanAOD, b3eed)
abline(a=-0.05, b=0.85, lty=2)
#dt <- lm(ModAod ~MeanAOD, QA3DTM)
#summary(dt)
abline(B2Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.9,y=0.22, "% within EE = 69.3 \n% above EE = 27.4 \n% below EE = 3.3 \nN = 1,337; r=0.46 \nY=0.04 + 0.94 (purple line)", cex=1.3)
dev.off()


tiff("H:/Rotation_Yang/Imagery/DT1Pic.tiff")
scatterplot.density(x=QA1DTM$MeanAOD, y=QA1DTM$ModAod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="10km Dark-Target (Marginal observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eeup)
abline(a=0.05, b=1.15, lty=2, col=rgb(0,0,0,0.6))
#line(QA3DTM$MeanAOD, dt3eed)
abline(a=-0.05, b=0.85, lty=2, col=rgb(0,0,0,0.6))
#dt <- lm(ModAod ~MeanAOD, QA3DTM)
#summary(dt)
abline(DT1Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.92,y=0.22, "% within EE = 61.0 \n% above EE = 35.5 \n% below EE = 3.5 \nN = 2,809; r=0.68 \nY=0.04 + 1.20 (purple line)", cex=1.3)
dev.off()

tiff("H:/Rotation_Yang/Imagery/DB1Pic.tiff")
scatterplot.density(x=QA1DBM$MeanAOD, y=QA1DBM$ModAod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="10km Deep-Blue (Marginal observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1)
#line(QA3DBM$MeanAOD, db3eeup)
abline(a=0.04995, b=1.15071, lty=2)
#line(QA3DBM$MeanAOD, db3eed)
abline(a=-0.05004, b=0.85052, lty=2)
#db_no_int <- lm(ModAod ~MeanAOD+0, QA3DBM)
#summary(db_no_int)
abline(DB1Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.92,y=0.22, "% within EE = 58.6 \n% above EE = 39.8 \n% below EE = 1.6 \nN = 17,622; r=0.61 \nY=0.08 + 0.90 (purple line)", cex=1.3)
dev.off()

tiff("H:/Rotation_Yang/Imagery/B1Pic.tiff")
scatterplot.density(x=QA1BM$MeanAOD, y=QA1BM$ModAod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="10km Merged (Marginal observations)", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
abline(a=0, b=1)
#line(QA3BM$MeanAOD, b3eeup)
abline(a=0.05, b=1.15, lty=2)
#line(QA3BM$MeanAOD, b3eed)
abline(a=-0.05, b=0.85, lty=2)
#dt <- lm(ModAod ~MeanAOD, QA3DTM)
#summary(dt)
abline(B1Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
text(x=0.9,y=0.22, "% within EE = 89.6 \n% above EE = 5.6 \n% below EE = 4.8 \nN = 922; r=0.74 \nY=0.03 + 0.66 (purple line)", cex=1.3)
dev.off()

#tiff("D:/YangRotation/k3Pic.tiff")
#scatterplot.density(x=QA34kM$MeanAOD, y=QA34kM$ModAod, xylim=c(0,1.2),xlab=expression(italic("AERONET 550nm AOD")), ylab=expression(italic("MODIS 550nm AOD")), main="3km Dark-Target", num.bins=85, col=rev(brewer.pal(6, "RdGy")), col.regression.line=NULL, col.one.to.one.line=NULL, density.in.percent=F, cex.lab=1.3, cex.main=1.2, cex.axis=1.3)
#abline(a=0, b=1)
#line(QA34kM$MeanAOD, eeUp3k)
#abline(a=0.05, b=1.15, lty=2)
#line(QA34kM$MeanAOD, eeD3k)
#abline(a=-0.05, b=0.85, lty=2)
#dt <- lm(ModAod ~MeanAOD, QA3DTM)
#summary(dt)
#abline(k3Model, col=rgb(118/255,42/255,131/255, 0.7), lwd=2)
#text(x=0.92,y=0.22, "% within EE = 71.9 \n% above EE = 23.6 \n% below EE = 4.5 \nN = 6,822; r=0.79 \nY=0.02 + 1.10 (purple line)", cex=1.3)
#dev.off()

# Need to filter these to exclude correlations made off of fewer than 3 observations
QA3dt <- CorStation(QA3DTM)
QA2dt <- CorStation(QA2DTM)
QA3db <- CorStation(QA3DBM)
QA2db <- CorStation(QA2DBM)
QA1db <- CorStation(QA1DBM)
hist(QA3db$COR, breaks=30)
hist(QA2db$COR)
hist(QA1db$COR, breaks=30)
hist(QA3dt$COR, breaks=30)

plot(abs(QA1db$COR), QA1db$x)

cor(QA3DTM$ModAod, QA3DTM$MeanAOD)
cor(QA3DBM$ModAod, QA3DBM$MeanAOD)
cor(QA3BM$ModAod, QA3BM$MeanAOD)
cor(QA2DTM$ModAod, QA2DTM$MeanAOD)
cor(QA2DBM$ModAod, QA2DBM$MeanAOD)
cor(QA2BM$ModAod, QA2BM$MeanAOD)
cor(QA1DTM$ModAod, QA1DTM$MeanAOD)
cor(QA1DBM$ModAod, QA1DBM$MeanAOD)
cor(QA1BM$ModAod, QA1BM$MeanAOD)

summary(QA3DTM)
summary(QA3DBM)
summary(QA3BM)
summary(QA1DBM)

CorSeason(QA3DTM)
CorSeason(QA2DTM)
CorSeason(QA1DTM)
CorSeason(QA3DBM)
CorSeason(QA2DBM)
CorSeason(QA1DBM)
CorSeason(QA3BM)
CorSeason(QA2BM)

CorMonth(QA3DTM)
CorMonth(QA2DTM)
CorMonth(QA1DTM)
CorMonth(QA3DBM)
CorMonth(QA2DBM)
CorMonth(QA1DBM)
CorMonth(QA3BM)
CorMonth(QA2BM)

## ----------
## Look at East/West differences
## ----------
East_locs <- read.csv("H:/Rotation_Yang/EastWestSplit/EastLocs100Long.csv")[,2]
East <- rep(1, length(East_locs))
East <- cbind.data.frame(East_locs, East)

# Did find and replace to get california comparisons - replaced East with Calif in this section
Calif_locs <- read.csv("H:/Rotation_Yang/EastWestSplit/Calif_locs.csv")[,2]
Calif <- rep(1, length(Calif_locs))
Calif <- cbind.data.frame(Calif_locs, Calif)

DT <- merge(QA3DTM, Calif, by.x="AeroLoc", by.y="Calif_locs", all.x=T)
DT$Calif <- ifelse(is.na(DT$Calif), 0,1)
DTem <- lm(ModAod~MeanAOD, subset(DT, DT$Calif==1))
summary(DTem)
DTwm <- lm(ModAod~MeanAOD, subset(DT, DT$Calif==0))
summary(DTwm)
CorDT3 <- ddply(DT, .(as.factor(Calif)), CovStrat)
DT <- merge(QA2DTM, Calif, by.x="AeroLoc", by.y="Calif_locs", all.x=T)
DT$Calif <- ifelse(is.na(DT$Calif), 0,1)
CorDT2 <- ddply(DT, .(as.factor(Calif)), CovStrat)
DT <- merge(QA1DTM, Calif, by.x="AeroLoc", by.y="Calif_locs", all.x=T)
DT$Calif <- ifelse(is.na(DT$Calif), 0,1)
CorDT1 <- ddply(DT, .(as.factor(Calif)), CovStrat)
DTcor <- cbind.data.frame(rbind.data.frame(CorDT3, CorDT2, CorDT1), c(rep(3,2),rep(2,2),rep(1,2)))
colnames(DTcor) <- c("Calif", "DT", "QA code")

DB <- merge(QA3DBM, Calif, by.x="AeroLoc", by.y="Calif_locs", all.x=T)
DB$Calif <- ifelse(is.na(DB$Calif), 0,1)
DBem <- lm(ModAod~MeanAOD, subset(DB, DB$Calif==1))
summary(DBem)
DBwm <- lm(ModAod~MeanAOD, subset(DB, DB$Calif==0))
summary(DBwm)
ddply(DB, .(as.factor(Calif)), CovStrat)
summary(as.factor(DB$Calif))
CorDB3 <- ddply(DB, .(as.factor(Calif)), CovStrat)
DB <- merge(QA2DBM, Calif, by.x="AeroLoc", by.y="Calif_locs", all.x=T)
DB$Calif <- ifelse(is.na(DB$Calif), 0,1)
CorDB2 <- ddply(DB, .(as.factor(Calif)), CovStrat)
DB <- merge(QA1DBM, Calif, by.x="AeroLoc", by.y="Calif_locs", all.x=T)
DB$Calif <- ifelse(is.na(DB$Calif), 0,1)
CorDB1 <- ddply(DB, .(as.factor(Calif)), CovStrat)
DBcor <- rbind.data.frame(CorDB3, CorDB2, CorDB1)[,2]

B <- merge(QA3BM, Calif, by.x="AeroLoc", by.y="Calif_locs", all.x=T)
B$Calif <- ifelse(is.na(B$Calif), 0,1)
Bem <- lm(ModAod~MeanAOD, subset(B, B$Calif==1))
summary(Bem)
Bwm <- lm(ModAod~MeanAOD, subset(B, B$Calif==0))
summary(Bwm)
ddply(B, .(as.factor(Calif)), CovStrat)
summary(as.factor(B$Calif))
CorB3 <- ddply(B, .(as.factor(Calif)), CovStrat)
B <- merge(QA2BM, Calif, by.x="AeroLoc", by.y="Calif_locs", all.x=T)
B$Calif <- ifelse(is.na(B$Calif), 0,1)
CorB2 <- ddply(B, .(as.factor(Calif)), CovStrat)
B <- merge(QA1BM, Calif, by.x="AeroLoc", by.y="Calif_locs", all.x=T)
B$Calif <- ifelse(is.na(B$Calif), 0,1)
CorB1 <- ddply(B, .(as.factor(Calif)), CovStrat)
Bcor <- rbind.data.frame(CorB3, CorB2, CorB1)[,2]


CorCalif <- cbind.data.frame(DTcor, DBcor, Bcor)
colnames(CorCalif) <- c("Calif", "DT", "QA code", "DB", "Combined")
CorCalif[,c(3,1,2,4,5)]



k3 <- merge(QA34kM, East, by.x="AeroLoc", by.y="East_locs", all.x=T)
k3$East <- ifelse(is.na(k3$East), 0,1)
k3em <- lm(ModAod~MeanAOD, subset(k3, k3$East==1))
summary(k3em)
k3wm <- lm(ModAod~MeanAOD, subset(k3, k3$East==0))
summary(k3wm)
ddply(k3, .(as.factor(East)), CovStrat)
summary(as.factor(k3$East))

## ---------------
## Create files for matchup with ground parameters
## ---------------
Modis04 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2004.csv")[,c(1:3, 11:21)]
Modis05 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2005.csv")[,c(1:3, 11:21)]
Modis06 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2006.csv")[,c(1:3, 11:21)]
Modis07 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2007.csv")[,c(1:3, 11:21)]
Modis08 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2008.csv")[,c(1:3, 11:21)]
Modis09 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2009.csv")[,c(1:3, 11:21)]
Modis10 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2010.csv")[,c(1:3, 11:21)]
Modis11 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2011.csv")[,c(1:3, 11:21)]
Modis12 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2012.csv")[,c(1:3, 11:21)]
Modis13 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2013.csv")[,c(1:3, 11:21)]
ModisStack <- rbind.data.frame(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13)
ModisStack$Year <- c(rep(2004, nrow(Modis04)), rep(2005, nrow(Modis05)), rep(2006, nrow(Modis06)), rep(2007, nrow(Modis07)), rep(2008, nrow(Modis08)), rep(2009, nrow(Modis09)), rep(2010, nrow(Modis10)), rep(2011, nrow(Modis11)), rep(2012, nrow(Modis12)), rep(2013, nrow(Modis13)))
rm(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13)

QA1BLL <- subset(ModisStack, ModisStack$QAb==1)[,c(1:3,7:15)]
QA1B <- merge(QA1BLL, QA1BM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA1B, "H:/Rotation_Yang/ForLink_QA1B.csv")
QA2BLL <- subset(ModisStack, ModisStack$QAb==2)[,c(1:3,7:15)]
QA2B <- merge(QA2BLL, QA2BM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA2B, "H:/Rotation_Yang/ForLink_QA2B.csv")
QA3BLL <- subset(ModisStack, ModisStack$QAb==3)[,c(1:3,7:15)]
QA3B <- merge(QA3BLL, QA3BM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA3B, "H:/Rotation_Yang/ForLink_QA3B.csv")

QA1DBLL <- subset(ModisStack, ModisStack$QAdb==1)[,c(1:3,7:15)]
QA1DB <- merge(QA1DBLL, QA1DBM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA1DB, "H:/Rotation_Yang/ForLink_QA1DB.csv")
QA2DBLL <- subset(ModisStack, ModisStack$QAdb==2)[,c(1:3,7:15)]
QA2DB <- merge(QA2DBLL, QA2DBM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA2DB, "H:/Rotation_Yang/ForLink_QA2DB.csv")
QA3DBLL <- subset(ModisStack, ModisStack$QAdb==3)[,c(1:3,7:15)]
QA3DB <- merge(QA3DBLL, QA3DBM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA3DB, "H:/Rotation_Yang/ForLink_QA3DB.csv")

QA0DTLL <- subset(ModisStack, ModisStack$QAdt==0)[,c(1:3,7:15)]
QA0DT <- merge(QA0DTLL, QA0DTM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA0DT, "H:/Rotation_Yang/ForLink_QA0DT.csv")
QA1DTLL <- subset(ModisStack, ModisStack$QAdt==1)[,c(1:3,7:15)]
QA1DT <- merge(QA1DTLL, QA1DTM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA1DT, "H:/Rotation_Yang/ForLink_QA1DT.csv")
QA2DTLL <- subset(ModisStack, ModisStack$QAdt==2)[,c(1:3,7:15)]
QA2DT <- merge(QA2DTLL, QA2DTM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA2DT, "H:/Rotation_Yang/ForLink_QA2DT.csv")
QA3DTLL <- subset(ModisStack, ModisStack$QAdt==3)[,c(1:3,7:15)]
QA3DT <- merge(QA3DTLL, QA3DTM, by=c("Year", "AeroLoc", "JulianDate", "Passtime"))
write.csv(QA3DT, "H:/Rotation_Yang/ForLink_QA3DT.csv")

## -----------
# Analysis of data with ground parameters - DT2
## -----------

MatchDT2 <- read.csv("H:/Rotation_Yang/Linked_QA3DT.csv")[,2:12]
DT2 <- merge(QA3DTM, MatchDT2, by=c("AeroLoc", "JulianDate", "Passtime", "Year"))

library(ggplot2)
library(plyr)

tiff("H:/Rotation_Yang/Imagery/ElevVar.tiff")
ggplot(DT2, aes(x=AeroLoc, y=MedElev)) + geom_point() + ylab("Median Elevation (m)") + xlab("AERONET site") + scale_x_discrete(labels=NULL) + ggtitle("Within- and between-site variability in median elevation") + theme_classic()
dev.off()
ggplot(DT2, aes(x=AeroLoc, y=MadElev)) + geom_point() + ylab("Median Absolute Deviation in Elevation") + xlab("AERONET site") + scale_x_discrete(labels=NULL) + theme_classic()
tiff("H:/Rotation_Yang/Imagery/SlopeVar.tiff")
ggplot(DT2, aes(x=AeroLoc, y=MedSlope)) + geom_point() + ylab("Median Slope (degrees)") + xlab("AERONET site") + scale_x_discrete(labels=NULL) + ggtitle("Within- and between-site variability in median slope") + theme_classic()
dev.off()
plot(MedElev~MedSlope, DT2)
plot(MadElev~MadSlope, DT2)
cor(DT2$MadElev, DT2$MadSlope)
cor(DT2$MedElev, DT2$MedSlope)
ggplot(DT2, aes(x=AeroLoc, y=MadSlope)) + geom_point() + ylab("Median Absolute Deviation in Slope") + xlab("AERONET site") + scale_x_discrete(labels=NULL) + theme_classic()
#ggplot(DT2, aes(x=AeroLoc, y=as.factor(ModeNLCD11))) + geom_point() + ylab("Mode of Landcover (NLCD11)") + xlab("AERONET site") + scale_x_discrete(labels=NULL) + theme_bw()
plot(ModeNLCD11~ModeNLCD06, DT2)
View(tapply(DT2[,20], DT2$AeroLoc, range))
DT2$Landcover <- ifelse(DT2$Year > 2008, DT2$ModeNLCD11, DT2$ModeNLCD06)
ggplot(DT2, aes(x=AeroLoc, y=as.factor(Landcover))) + geom_point() + ylab("Landcover") + xlab("AERONET site") + scale_x_discrete(labels=NULL) + theme_bw()
tiff("H:/Rotation_Yang/Imagery/PopVar.tiff")
ggplot(DT2, aes(x=AeroLoc, y=PopSum)) + geom_point() + ylab("Total Population") + xlab("AERONET site") + scale_x_discrete(labels=NULL) + ggtitle("Within- and between-site variability in total population") + theme_classic()
dev.off()
DT2ModelG <- lm(ModAod~MeanAOD + as.factor(Landcover) + PopSum + MedElev, DT2)
summary(DT2ModelG)
DT2ModelG_Base <- lm(ModAod~MeanAOD, DT2)
summary(DT2ModelG_Base)


## ---------------
## MatchOver - looking for NDVI dependence in Aeronet/MODIS relationship
## ---------------


DirDT3Match <- list.files("H://Rotation_Yang/MatchOver/DT3", full.names=T)
for (file in DirDT3Match){
  fileR <- read.csv(file)[,2:6]
  QA3DTM <- merge(QA3DTM, fileR, by=c("AeroLoc", "JulianDate", "Passtime", "Year"))
}
colnames(QA3DTM) <- c(colnames(QA3DTM[1:15]), unlist(strsplit(list.files("H://Rotation_Yang/MatchOver/DT3"), "[.]"))[c(1,3,5,7,9,11,13,15)])
str(QA3DTM)

QA3DTM$Passtime2 <- cut(QA3DTM$Passtime/5, breaks=c(300, 332, 352, 372, 392, 412, 432, 452), labels=c(1,2,3,4,5,6,7))
QA3DTM$PasstimeBreak <- cut(as.numeric(QA3DTM$Passtime2), breaks=c(0,3.5,5.5,8), labels=c("E", "M", "W"))
ggplot(QA3DTM, aes(x=MeanAOD, y=ModAod)) + geom_point() + ylim(c(0,2.5)) + xlim(c(0,2.5)) + facet_grid(PasstimeBreak~.) + theme_classic()

East_locs <- read.csv("H:/Rotation_Yang/EastWestSplit/EastLocs100Long.csv")[,2]
East <- rep(1, length(East_locs))
East <- cbind.data.frame(East_locs, East)
QA3DTM <- merge(QA3DTM, East, by.x="AeroLoc", by.y="East_locs", all.x=T)
QA3DTM$East <- ifelse(is.na(QA3DTM$East), 0,1)

CASTNET <- read.csv("H:/Rotation_Yang/HygroscopicAnal/Aeronet_castnet_approx.csv")[,c("Location", "CASTNET_12", "CASTNET_18")]
QA3DTM <- merge(QA3DTM, CASTNET, by.x="AeroLoc", by.y="Location", all.x=T)

QA3DTM$WaterCat <- cut(QA3DTM$Watercm, breaks= quantile(QA3DTM$Watercm, probs=seq(0,1,0.2), na.rm=T, names=F), labels=seq(1,5), include.lowest=T)
summary(lm(ModAod~(MeanAOD + WaterCat + CASTNET_12 + CASTNET_18)^2, subset(QA3DTM, QA3DTM$East=="East")))
summary(lm(ModAod~(MeanAOD + WaterCat + CASTNET_18 + CASTNET_12)^2 - WaterCat*CASTNET_12 - WaterCat*CASTNET_18, subset(QA3DTM, QA3DTM$East=="West")))
summary(lm(ModAod~MeanAOD + WaterCat + CASTNET_18 + CASTNET_12 + WaterCat*CASTNET_18 + CASTNET_12*CASTNET_18, subset(QA3DTM, QA3DTM$East=="West")))

QA3DTM$East <- cut(QA3DTM$East, breaks=c(-0.5, 0.5, 1.5), labels=c("West", "East"))
ggplot(QA3DTM, aes(x=MeanAOD, y=ModAod, z=NDVI)) + 
  geom_abline(intercept=0, slope=1, col="gray80", linetype="dashed") + 
  stat_summary2d(bins=100, fun=median) + 
  facet_grid(East~.) + 
  #scale_fill_gradient2("NDVI", low=muted("purple"), high=muted("green"), mid="gray90", midpoint=0.42, limits=c(0,1)) +
  scale_fill_gradientn("NDVI", colours=c("black", muted("purple"), "gray90", muted("green")), values=c(0, 0.25, 0.45, 1)) +
  scale_x_continuous("Aeronet AOD", limits=c(0,2)) + 
  scale_y_continuous("MODIS AOD", limits=c(0,2)) + 
  theme_classic() 

summary(lm(ModAod~MeanAOD, subset(QA3DTM, QA3DTM$East=="East")))
summary(lm(ModAod~MeanAOD, subset(QA3DTM, QA3DTM$East=="West")))

## DB

DirDB3Match <- list.files("H://Rotation_Yang/MatchOver/DB3", full.names=T)
for (file in DirDB3Match){
  fileR <- read.csv(file)[,2:6]
  QA3DBM <- merge(QA3DBM, fileR, by=c("AeroLoc", "JulianDate", "Passtime", "Year"))
}
colnames(QA3DBM) <- c(colnames(QA3DBM[1:15]), unlist(strsplit(list.files("H://Rotation_Yang/MatchOver/DB3"), "[.]"))[c(1,3,5,7,9,11,13,15,17,19)])
str(QA3DBM)

QA3DBM$Passtime2 <- cut(QA3DBM$Passtime/5, breaks=c(300, 332, 352, 372, 392, 412, 432, 452), labels=c(1,2,3,4,5,6,7))
QA3DBM$PasstimeBreak <- cut(as.numeric(QA3DBM$Passtime2), breaks=c(0,3.5,5.5,8), labels=c("E", "M", "W"))
ggplot(QA3DBM, aes(x=MeanAOD, y=ModAod)) + geom_point() + ylim(c(0,2.5)) + xlim(c(0,2.5)) + facet_grid(PasstimeBreak~.) + theme_classic()

East_locs <- read.csv("H:/Rotation_Yang/EastWestSplit/EastLocs.csv")[,2]
East <- rep(1, length(East_locs))
East <- cbind.data.frame(East_locs, East)
QA3DBM <- merge(QA3DBM, East, by.x="AeroLoc", by.y="East_locs", all.x=T)
QA3DBM$East <- ifelse(is.na(QA3DBM$East), 0,1)

QA3DBM$East <- cut(QA3DBM$East, breaks=c(-0.5, 0.5, 1.5), labels=c("West", "East"))
ggplot(QA3DBM, aes(x=MeanAOD, y=ModAod, z=Watercm)) + 
  geom_abline(intercept=0, slope=1, col="gray80", linetype="dashed") + 
  stat_summary2d(bins=100, fun=median) + 
  facet_grid(East~.) + 
  #scale_fill_gradient2("NDVI", low=muted("purple"), high=muted("green"), mid="gray90", midpoint=0.42, limits=c(0,1)) +
  scale_fill_gradientn("Watercm", colours=c("black", muted("purple"), "gray90", muted("green"))) +
  scale_x_continuous("Aeronet AOD", limits=c(0,1)) + 
  scale_y_continuous("MODIS AOD", limits=c(0,1)) + 
  theme_classic() 

CASTNET <- read.csv("H:/Rotation_Yang/HygroscopicAnal/Aeronet_castnet_approx.csv")[,c("Location", "CASTNET_12", "CASTNET_18")]
QA3DBM <- merge(QA3DBM, CASTNET, by.x="AeroLoc", by.y="Location", all.x=T)
Red <- read.csv("H:/Rotation_Yang/Iron_red/Export_Output.csv")[,c("Location", "RASTERVALU")]
QA3DBM <- merge(QA3DBM, Red, by.x="AeroLoc", by.y="Location", all.x=T)

QA3DBM$WaterCat <- cut(QA3DBM$Watercm, breaks= quantile(QA3DBM$Watercm, probs=seq(0,1,0.2), na.rm=T, names=F), labels=seq(1,5), include.lowest=T)
summary(lm(ModAod~(MeanAOD + WaterCat + CASTNET_18)^2, subset(QA3DBM, QA3DBM$East=="East")))
summary(lm(ModAod~(MeanAOD + WaterCat + CASTNET_12)^2, subset(QA3DBM, QA3DBM$East=="West")))


summary(lm(ModAod~MeanAOD, subset(QA3DBM, QA3DBM$East=="East")))
summary(lm(ModAod~MeanAOD, subset(QA3DBM, QA3DBM$East=="West")))
# No Intercept
summary(lm(ModAod~0+MeanAOD, subset(QA3DBM, QA3DBM$East=="East")))
summary(lm(ModAod~0+MeanAOD, subset(QA3DBM, QA3DBM$East=="West")))

aggregate(QA3DBM$NDVI, by=list(QA3DBM$East), "mean")
aggregate(QA3DTM$NDVI, by=list(QA3DTM$East), "mean", na.rm=T)


QA3DBM$NDVIp1cuts <- cut(QA3DBM$NDVI, breaks=seq(-0.2, 1, 0.1), labels=seq(1,12))
QA3DBM$NDVIdeciles <- cut(QA3DBM$NDVI, breaks= quantile(QA3DBM$NDVI, probs=seq(0,1,0.1), na.rm=T, names=F), labels=seq(1,10), include.lowest=T)

SubPass2 <- subset(QA3DBM, QA3DBM$East == "West")
#SubPass2 <- subset(QA3DTM, QA3DTM$PasstimeBreak == "W")
for (i in seq(1, 12)){
  GSub <- subset(SubPass2, SubPass2$NDVIp1cuts == as.character(i))
  plot(GSub$ModAod~GSub$MeanAOD, ylim=c(0,2.5), xlim=c(0,2.5), main=paste("Part", i))
  abline(a=0, b=1)
  print(summary(lm(GSub$ModAod~GSub$MeanAOD)))
}
for (i in seq(1, 10)){
  GSub <- subset(SubPass2, SubPass2$NDVIdeciles == as.character(i))
  plot(GSub$ModAod~GSub$MeanAOD, ylim=c(0,2.5), xlim=c(0,2.5), main=paste("Decile", i))
  abline(a=0, b=1)
  print(summary(lm(GSub$ModAod~GSub$MeanAOD)))
}

QA3DBM$NDVIdecilesBreaks <- cut(as.numeric(QA3DBM$NDVIdeciles), breaks=c(0,1.5,3.5,7.5,10.5), labels=seq(1,4))

QA3DBM$NDVIdecilesBreaks <- cut(QA3DBM$NDVIdeciles, breaks=c(0,1.5,3.5, 7.5, 10.5), labels=seq(1,4))
ggplot(subset(QA3DBM, QA3DBM$East=="West"), aes(x=MeanAOD, y=ModAod)) + geom_point(alpha=0.5) + geom_abline(intercept=0, slope=1, col="gray80", linetype="dashed") + facet_grid(NDVIdecilesBreaks~.) + theme_classic() 

## -------------------
# Extreme values analysis - Threshold
## -------------------

Extreme_thresh <- rbind.data.frame(ExVals(QA3DTM$MeanAOD, QA3DTM$ModAod), ExVals(QA1DTM$MeanAOD, QA1DTM$ModAod), ExVals(QA3DBM$MeanAOD, QA3DBM$ModAod), ExVals(QA1DBM$MeanAOD, QA1DBM$ModAod), ExVals(QA3BM$MeanAOD, QA3BM$ModAod), ExVals(QA1BM$MeanAOD, QA1BM$ModAod))
Extreme_thresh$Alg <- c(rep("DT", 2), rep("DB", 2), rep("B", 2))
Extreme_thresh$QAC <- rep(c(3,1), 3)
Extreme_thresh

East_locs <- read.csv("H:/Rotation_Yang/EastWestSplit/EastLocs100Long.csv")[,3]
East <- rep(1, length(East_locs))
East <- cbind.data.frame(East_locs, East)
QA3DTM <- AddEW(East, QA3DTM)
QA1DTM <- AddEW(East, QA1DTM)
QA3DBM <- AddEW(East, QA3DBM)
QA1DBM <- AddEW(East, QA1DBM)
QA3BM <- AddEW(East, QA3BM)
QA1BM <- AddEW(East, QA1BM)
Extreme <- rbind.data.frame(ExVals(QA3DTM$MeanAOD, QA3DTM$ModAod, thresh=c(quantile(QA3DTM$MeanAOD[QA3DTM$East=="West"], probs=0.95), quantile(QA3DTM$MeanAOD[QA3DTM$East=="East"], probs=0.95)), subseter=QA3DTM$East), ExVals(QA1DTM$MeanAOD, QA1DTM$ModAod, thresh=c(quantile(QA1DTM$MeanAOD[QA1DTM$East=="West"], probs=0.95), quantile(QA1DTM$MeanAOD[QA1DTM$East=="East"], probs=0.95)), subseter=QA1DTM$East), ExVals(QA3DBM$MeanAOD, QA3DBM$ModAod, thresh=c(quantile(QA3DBM$MeanAOD[QA3DBM$East=="West"], probs=0.95), quantile(QA3DBM$MeanAOD[QA3DBM$East=="East"], probs=0.95)), subseter=QA3DBM$East), ExVals(QA1DBM$MeanAOD, QA1DBM$ModAod, thresh=c(quantile(QA1DBM$MeanAOD[QA1DBM$East=="West"], probs=0.95), quantile(QA1DBM$MeanAOD[QA1DBM$East=="East"], probs=0.95)), subseter=QA1DBM$East), ExVals(QA3BM$MeanAOD, QA3BM$ModAod, thresh=c(quantile(QA3BM$MeanAOD[QA3BM$East=="West"], probs=0.95), quantile(QA3BM$MeanAOD[QA3BM$East=="East"], probs=0.95)), subseter=QA3BM$East), ExVals(QA1BM$MeanAOD, QA1BM$ModAod, thresh=c(quantile(QA1BM$MeanAOD[QA1BM$East=="West"], probs=0.95), quantile(QA1BM$MeanAOD[QA1BM$East=="East"], probs=0.95)), subseter=QA1BM$East))
Extreme$Alg <- c(rep("DT", 4), rep("DB", 4), rep("B", 4))
Extreme$QAC <- rep(c(3,3,1,1), 3)
format(Extreme, digits=3)

Extreme0p5 <- rbind.data.frame(ExVals(QA3DTM$MeanAOD, QA3DTM$ModAod, thresh=c(0.5, 0.5), subseter=QA3DTM$East), ExVals(QA1DTM$MeanAOD, QA1DTM$ModAod, thresh=c(0.5, 0.5), subseter=QA1DTM$East), ExVals(QA3DBM$MeanAOD, QA3DBM$ModAod, thresh=c(0.5, 0.5), subseter=QA3DBM$East), ExVals(QA1DBM$MeanAOD, QA1DBM$ModAod, thresh=c(0.5, 0.5), subseter=QA1DBM$East), ExVals(QA3BM$MeanAOD, QA3BM$ModAod, thresh=c(0.5, 0.5), subseter=QA3BM$East), ExVals(QA1BM$MeanAOD, QA1BM$ModAod, thresh=c(0.5, 0.5), subseter=QA1BM$East))
Extreme0p5$Alg <- c(rep("DT", 4), rep("DB", 4), rep("B", 4))
Extreme0p5$QAC <- rep(c(3,3,1,1), 3)
format(Extreme0p5, digits=3)

## --------------
# Test parametric models for Deep Blue (accounting for truncation around 0)
## --------------

# Base model
AIC(lm(ModAod~MeanAOD, data=QA3DBM))
# Base model with no intercept
summary(lm(ModAod~0 + MeanAOD, data=QA3DBM))
# Truncated normal distribution assumed on y
library(truncreg)
summary(truncreg(ModAod~MeanAOD, data=QA3DBM))
sum(ifelse(QA3DBM$MeanAOD == 0, 1,0))
# Tobit model
library(VGAM)
summary(vglm(ModAod~MeanAOD, data=QA3DBM, family=tobit(Lower=0.075)))

## ---------------
# Accuracy analysis - final version
# Conducting - gold standard regression modeling - identification of optimal model - investigation into multicollinearity of predictors - predictor categorizations and transformations - creation of tables and funnel plots
## ---------------

QA3DT <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DT3", QA3DTM)
QA2DT <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DT2", QA2DTM)
QA1DT <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DT1", QA1DTM)
QA3DB <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DB3", QA3DBM)
QA2DB <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DB2", QA2DBM)
QA1DB <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DB1", QA1DBM)
QA3B <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/B3", QA3BM)
QA2B <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/B2", QA2BM)
QA1B <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/B1", QA1BM)

## First figure - panel of MODIS vs. AERONET AOD plots - by region



## Second figure - AOD error (MODIS AOD - AERONET AOD) vs. predictor values - by region

## Third figure - site-level correlations (prep dataset here, and map in Arc)

## Regression modeling - by region

## Table - regression modeling results

## Fourth figure - Funnel plots of regression results
