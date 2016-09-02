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
source("H:/Rotation_Yang/Fuctions_RadMatch10km.R")

## ---------
## Read in Aeronet data
## ---------

Aero11 <- read.csv("H:/Rotation_Yang/AERONETcollocs11_550Ints.csv")
Aero12 <- read.csv("H:/Rotation_Yang/AERONETcollocs12_550Ints.csv")
Aero13 <- read.csv("H:/Rotation_Yang/AERONETcollocs13_550Ints.csv")

Aero1 <- rbind.data.frame(Aero11, Aero12, Aero13)
Year <- c(rep(2011, nrow(Aero11)), rep(2012, nrow(Aero12)), rep(2013, nrow(Aero13)))
Aero <- na.omit(cbind.data.frame(Aero1, Year))
attributes(Aero)$na.action <- NULL
Aero$X <- NULL
Aero$Timestamp <- as.numeric(apply(Aero, 2, gsub, pattern=":", replacement="", x=Aero$Timestamp)[,1])
AOD550_exp <- exp(Aero$AOD550nm)
Aero <- cbind.data.frame(Aero, AOD550_exp)

rm(Aero11, Aero12, Aero13, AOD550_exp, Aero1, Year)

## -------
## Read in MODIS data and merge with AERONET
## -------

Modis11_S1 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch11_S1.csv")
Modis11_S2 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch11_S2.csv")
Modis11_S3 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch11_S3.csv")
Modis11_S4 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch11_S4.csv")
Modis11_S5 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch11_S5.csv")
Modis11_S6 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch11_S6.csv")
Modis11_S7 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch11_S7.csv")
Modis11_S8 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch11_S8.csv")
Modis11_S9 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch11_S9.csv")
Modis11 <- rbind.data.frame(Modis11_S1, Modis11_S2, Modis11_S3, Modis11_S4, Modis11_S5, Modis11_S6, Modis11_S7, Modis11_S8, Modis11_S9)
rm(Modis11_S1, Modis11_S2, Modis11_S3, Modis11_S4, Modis11_S5, Modis11_S6, Modis11_S7, Modis11_S8, Modis11_S9)

Modis12_S1 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch12_S1.csv")
Modis12_S2 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch12_S2.csv")
Modis12_S3 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch12_S3.csv")
Modis12_S4 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch12_S4.csv")
Modis12_S5 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch12_S5.csv")
Modis12_S6 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch12_S6.csv")
Modis12_S7 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch12_S7.csv")
Modis12_S8 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch12_S8.csv")
Modis12_S9 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch12_S9.csv")
Modis12 <- rbind.data.frame(Modis12_S1, Modis12_S2, Modis12_S3, Modis12_S4, Modis12_S5, Modis12_S6, Modis12_S7, Modis12_S8, Modis12_S9)
rm(Modis12_S1, Modis12_S2, Modis12_S3, Modis12_S4, Modis12_S5, Modis12_S6, Modis12_S7, Modis12_S8, Modis12_S9)

Modis13_S1 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch13_S1.csv")
Modis13_S2 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch13_S2.csv")
Modis13_S3 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch13_S3.csv")
Modis13_S4 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch13_S4.csv")
Modis13_S5 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch13_S5.csv")
Modis13_S6 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch13_S6.csv")
Modis13_S7 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch13_S7.csv")
Modis13_S8 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch13_S8.csv")
Modis13_S9 <- read.csv("H:/Rotation_Yang/MODIS3km_RadMatch13_S9.csv")
Modis13 <- rbind.data.frame(Modis13_S1, Modis13_S2, Modis13_S3, Modis13_S4, Modis13_S5, Modis13_S6, Modis13_S7, Modis13_S8, Modis13_S9)
rm(Modis13_S1, Modis13_S2, Modis13_S3, Modis13_S4, Modis13_S5, Modis13_S6, Modis13_S7, Modis13_S8, Modis13_S9)
QA3DT <- StacksYrsDat(Modis11, Modis12, Modis13, 3, c(1:3,7:9))
QA3DTM <- MergeAero(Aero, QA3DT)
rm(QA3DT)
QA1DT <- StacksYrsDat(Modis11, Modis12, Modis13, 1, c(1:3,7:9))
QA1DTM <- MergeAero(Aero, QA1DT)
rm(QA1DT)
QA0DT <- StacksYrsDat(Modis11, Modis12, Modis13, 0, c(1:3,7:9))
QA0DTM <- MergeAero(Aero, QA0DT)
rm(QA0DT)

Modis <- rbind.data.frame(Modis11, Modis12, Modis13)
Modis$Year <- c(rep(2011, length(Modis11[,1])), rep(2012, length(Modis12[,1])), rep(2013, length(Modis13[,1])))
MODscale <- 0.0010000000474974513

rm(Modis11, Modis12, Modis13, Aero)
QA3DTM$AODavg <- QA3DTM$AODavg*MODscale
QA1DTM$AODavg <- QA1DTM$AODavg*MODscale
QA0DTM$AODavg <- QA0DTM$AODavg*MODscale

## ---------
## Run models on existing data and save results
## ---------
# Need to filter these to exclude correlations made off of fewer than 3 observations
QA3dt <- CorStation(QA3DTM)
QA1dt <- CorStation(QA1DTM)
QA0dt <- CorStation(QA0DTM)

QA3dt <- CorSeason(QA3DTM)
CorMonth(QA3DTM)

landuse <- read.csv("H:/Rotation_Yang/AERONET/ClassStations_NLCD/ZonalStats_LandUse.csv")
QA3Land <- merge(QA3DTM, landuse, by.x="AeroLoc", by.y="LOCATION")
DragonCov_QA3 <- ddply(QA3Land, .(as.factor(Dragon)), CovStrat)
ddply(QA3Land, .(as.factor(City)), CovStrat)
ddply(QA3Land, .(as.factor(MAJORITY)), CovStrat)


eeUp = QA3DTM$MeanAOD + (0.05 + (0.15*QA3DTM$MeanAOD))
eeD = QA3DTM$MeanAOD - (0.05 + (0.15*QA3DTM$MeanAOD))
AboveEE = ifelse(QA3DTM$AODavg > eeUp, 1, 0)
BelowEE = ifelse(QA3DTM$AODavg < eeD, 1, 0)
InEE = ifelse(AboveEE == 0 & BelowEE == 0, 1, 0)
sum(AboveEE)/length(AboveEE)
sum(BelowEE)/length(BelowEE)
sum(InEE)/length(InEE)
plot(AODavg~MeanAOD, QA3DTM, col="gray50")
abline(a=0, b=1)
lines(QA3DTM$MeanAOD, eeUp)
lines(QA3DTM$MeanAOD, eeD)
dt <- lm(AODavg~MeanAOD, QA3DTM)
summary(dt)
abline(dt, col="red")
## ----------
## Create dataset containing the the bounding coordinates for each pixel in the sample
## ----------


