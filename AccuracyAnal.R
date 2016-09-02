## ------------------
## Name: AccuracyAnal.R
## Program version: R 3.1.0
## Dependencies: plyr, ggplot2, grid, scales, lattice, RColorBrewer, modeest, cowplot, gridExtra, quantreg
## Function file(s): Functions_RadMatch10km.r, Functions_RadMatch3km.R
## Author: J.H. Belle
## Purpose: Record of final accuracy analysis figures, modeling, and tables for 3km and 10 km products
## ------------------

# Load libraries and function files
library(plyr)
library(ggplot2)
library(grid)
library(scales)
library(modeest)
library(cowplot)
library(gridExtra)
library(quantreg)
source("H:/Rotation_Yang/Fuctions_RadMatch10km.r")
source("H:/Rotation_Yang/Fuctions_RadMatch3km.r")

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

## ---------
# Read in 10 km MODIS data and process collocations
## ---------

Modis04 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2004.csv")[,1:13]
Mod04Angles <- read.csv("H:/Rotation_Yang/MODISExRadMatchAngles_2004.csv")[,c(1:5,7:11)]
Modis04 <- merge(Modis04, Mod04Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))
Modis05 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2005.csv")[,1:13]
Mod05Angles <- read.csv("H:/Rotation_Yang/MODISExRadMatchAngles_2005.csv")[,c(1:5,7:11)]
Modis05 <- merge(Modis05, Mod05Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))
Modis06 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2006.csv")[,1:13]
Mod06Angles <- read.csv("H:/Rotation_Yang/MODISExRadMatchAngles_2006.csv")[,c(1:5,7:11)]
Modis06 <- merge(Modis06, Mod06Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))
Modis07 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2007.csv")[,1:13]
Mod07Angles <- read.csv("H:/Rotation_Yang/MODISExRadMatchAngles_2007.csv")[,c(1:5,7:11)]
Modis07 <- merge(Modis07, Mod07Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))
Modis08 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2008.csv")[,1:13]
Mod08Angles <- read.csv("H:/Rotation_Yang/MODISExRadMatchAngles_2008.csv")[,c(1:5,7:11)]
Modis08 <- merge(Modis08, Mod08Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))
Modis09 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2009.csv")[,1:13]
Mod09Angles <- read.csv("H:/Rotation_Yang/MODISExRadMatchAngles_2009.csv")[,c(1:5,7:11)]
Modis09 <- merge(Modis09, Mod09Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))
Modis10 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2010.csv")[,1:13]
Mod10Angles <- read.csv("H:/Rotation_Yang/MODISExRadMatchAngles_2010.csv")[,c(1:5,7:11)]
Modis10 <- merge(Modis10, Mod10Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))
Modis11 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2011.csv")[,1:13]
Mod11Angles <- read.csv("H:/Rotation_Yang/MODISExRadMatchAngles_2011.csv")[,c(1:5,7:11)]
Modis11 <- merge(Modis11, Mod11Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))
Modis12 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2012.csv")[,1:13]
Mod12Angles <- read.csv("H:/Rotation_Yang/MODISExRadMatchAngles_2012.csv")[,c(1:5,7:11)]
Modis12 <- merge(Modis12, Mod12Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))
Modis13 <- read.csv("H:/Rotation_Yang/MODISExRadMatch_2013.csv")[,1:13]
Mod13Angles <- read.csv("H:/Rotation_Yang/MODISExRadMatchAngles_2013.csv")[,c(1:5,7:11)]
Modis13 <- merge(Modis13, Mod13Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))
rm(Mod04Angles, Mod05Angles, Mod06Angles, Mod07Angles, Mod08Angles, Mod09Angles, Mod10Angles, Mod11Angles, Mod12Angles, Mod13Angles)
gc()
QA3DT <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 3, c(1:3,7,10,11,14:18))
QA3DTM <- MergeAero(Aero, QA3DT)
rm(QA3DT)
QA2DT <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 2, c(1:3,7,10,11,14:18))
QA2DTM <- MergeAero(Aero, QA2DT)
rm(QA2DT)
QA1DT <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 1, c(1:3,7,10,11,14:18))
QA1DTM <- MergeAero(Aero, QA1DT)
rm(QA1DT)
QA3DB <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 3, c(1:3,8,10,12,14:18))
QA3DBM <- MergeAero(Aero, QA3DB)
rm(QA3DB)
QA2DB <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 2, c(1:3,8,10,12,14:18))
QA2DBM <- MergeAero(Aero, QA2DB)
rm(QA2DB)
QA1DB <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 1, c(1:3,8,10,12,14:18))
QA1DBM <- MergeAero(Aero, QA1DB)
rm(QA1DB)
QA3B <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 3, c(1:3,9,10,13,14:18))
QA3BM <- MergeAero(Aero, QA3B)
rm(QA3B)
QA2B <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 2, c(1:3,9,10,13,14:18))
QA2BM <- MergeAero(Aero, QA2B)
rm(QA2B)
QA1B <- StacksYrsDat(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13, 1, c(1:3,9,10,13,14:18))
QA1BM <- MergeAero(Aero, QA1B)
rm(QA1B)
rm(Modis04, Modis05, Modis06, Modis07, Modis08, Modis09, Modis10, Modis11, Modis12, Modis13)

## ------
# Read in 3 km MODIS data and process collocations
## ------

Filelist <- list.files("H:/Rotation_Yang/MOD3kmMatch", full.names=T)
FileAngles <- list.files("H:/Rotation_Yang/MOD3kmAngles", full.names=T)
FileAngles <- cbind.data.frame(FileAngles, as.integer(paste("20", substr(FileAngles, 48,49), sep="")))
colnames(FileAngles) <- c("Files", "Years")
for (f in Filelist){
  Year <- as.integer(paste("20", substr(f, 47, 48), sep=""))
  MODdat <- read.csv(f)[,c(1:5,6:9)]
  MODdat <- subset(MODdat, MODdat$DistStation < 7500)
  Angles <- read.csv(as.character(FileAngles$Files[which(FileAngles$Years == Year)]))[,c(1:5,7:11)]
  MODdat <- merge(MODdat, Angles, by=c("AeroLoc", "JulianDate", "Passtime", "LatMatch", "LongMatch"))[,c(1:3,6:14)]
  Ntot <- aggregate(MODdat[,5], list(MODdat$AeroLoc, MODdat$JulianDate, MODdat$Passtime), length)
  QA3 <- try(merge(GetQAdat3km(MODdat, 3), Ntot, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3")))
  if (is.data.frame(QA3)) {
    QA3$Year <- rep(Year, nrow(QA3))
    QA3$PercAvail <- (QA3$NQA/QA3$x)*100
    DT3 <- subset(QA3, QA3$PercAvail >= 20)
    if (exists("Outp3")) {
      Outp3 <- rbind.data.frame(Outp3, DT3)
    } else Outp3=DT3
  }
  rm(DT3, QA3)
  QA1 <- try(merge(GetQAdat3km(MODdat, 1), Ntot, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3")))
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
DT33kM <- MergeAero3km(Aero, Outp3)
DT13kM <- MergeAero3km(Aero, Outp1)

rm(Outp3, Outp0, Outp1, Aero, Filelist, f, Angles, FileAngles)
gc()
## -------
# Scale MODIS values
## -------

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
DT33kM$AODMod <- DT33kM$AODMod*MODscale
DT13kM$AODMod <- DT13kM$AODMod*MODscale

Anglescale <- 0.009999999776482582
## Scale AOD
QA1BM[,8:12] <- QA1BM[,8:12]*Anglescale
QA1DBM[,8:12] <- QA1DBM[,8:12]*Anglescale
QA1DTM[,8:12] <- QA1DTM[,8:12]*Anglescale
QA2BM[,8:12] <- QA2BM[,8:12]*Anglescale
QA2DBM[,8:12] <- QA2DBM[,8:12]*Anglescale
QA2DTM[,8:12] <- QA2DTM[,8:12]*Anglescale
QA3BM[,8:12] <- QA3BM[,8:12]*Anglescale
QA3DBM[,8:12] <- QA3DBM[,8:12]*Anglescale
QA3DTM[,8:12] <- QA3DTM[,8:12]*Anglescale
DT33kM[,8:12] <- DT33kM[,8:12]*Anglescale
DT13kM[,8:12] <- DT13kM[,8:12]*Anglescale
rm(Anglescale, MODscale)
## -------
# Add in ancillary information on collocation ground parameters and retrieval conditions
## -------

QA3DT <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DT3", QA3DTM)
QA2DT <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DT2", QA2DTM)
QA1DT <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DT1", QA1DTM)
QA3DB <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DB3", QA3DBM)
QA2DB <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DB2", QA2DBM)
QA1DB <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DB1", QA1DBM)
QA3B <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/B3", QA3BM)
QA2B <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/B2", QA2BM)
QA1B <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/B1", QA1BM)
DT33k <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DT33km", DT33kM)
DT13k <- AddAncillary("H:/Rotation_Yang/MatchOver/Intermediates_MatchOver/DT13km", DT13kM)

rm(QA3DTM, QA2DTM, QA1DTM, QA3DBM, QA2DBM, QA1DBM, QA3BM, QA2BM, QA1BM, DT33kM, DT13kM)
gc()

# Save copies of complete collocation files
#write.csv(QA3DT, "H:/Rotation_Yang/QA3DT.csv")
#write.csv(QA2DT, "H:/Rotation_Yang/QA2DT.csv")
#write.csv(QA1DT, "H:/Rotation_Yang/QA1DT.csv")
#write.csv(QA3DB, "H:/Rotation_Yang/QA3DB.csv")
#write.csv(QA2DB, "H:/Rotation_Yang/QA2DB.csv")
#write.csv(QA1DB, "H:/Rotation_Yang/QA1DB.csv")
#write.csv(QA3B, "H:/Rotation_Yang/QA3B.csv")
#write.csv(QA2B, "H:/Rotation_Yang/QA2B.csv")
#write.csv(QA1B, "H:/Rotation_Yang/QA1B.csv")
#write.csv(DT33k, "H:/Rotation_Yang/DT33k.csv")
#write.csv(DT13k, "H:/Rotation_Yang/DT13k.csv")

# Standardize column content and names across datasets and stack together - with additional variables for resolution, algorithm, and QAC code
colnames(QA3DT)
colnames(QA2DT)
colnames(QA1DT)
colnames(QA3DB)
colnames(QA2DB)
colnames(QA1DB)
colnames(QA3B)
colnames(QA2B)
colnames(QA1B)
colnames(DT33k)
colnames(DT13k)
QA3DT$NLCDMode <- ifelse(QA3DT$Year < 2009, QA3DT$NLCD11, QA3DT$NLCD06)
QA2DT$NLCDMode <- ifelse(QA2DT$Year < 2009, QA2DT$NLCD11, QA2DT$NLCD06)
QA1DT$NLCDMode <- ifelse(QA1DT$Year < 2009, QA1DT$NLCD11, QA1DT$NLCD06)
QA3DT$NLCD06 <- NULL
QA3DT$NLCD11 <- NULL
QA2DT$NLCD06 <- NULL
QA2DT$NLCD11 <- NULL
QA1DT$NLCD06 <- NULL
QA1DT$NLCD11 <- NULL
QA3DT$PImpervSum <- rep(NA, nrow(QA3DT))
QA2DT$PImpervSum <- rep(NA, nrow(QA2DT))
QA3DT <- QA3DT[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "ModAod", "MeanAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(QA3DT) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")
QA2DT <- QA2DT[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "ModAod", "MeanAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(QA2DT) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")
QA1DT <- QA1DT[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "ModAod", "MeanAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(QA1DT) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")
QA3DB <- QA3DB[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "ModAod", "MeanAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(QA3DB) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")
QA2DB <- QA2DB[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "ModAod", "MeanAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(QA2DB) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")
QA1DB <- QA1DB[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "ModAod", "MeanAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(QA1DB) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")
QA3B <- QA3B[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "ModAod", "MeanAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(QA3B) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")
QA2B <- QA2B[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "ModAod", "MeanAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(QA2B) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")
QA1B <- QA1B[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "ModAod", "MeanAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(QA1B) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")
DT33k <- DT33k[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "AODMod", "AeroAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(DT33k) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")
DT13k <- DT13k[c("AeroLoc", "JulianDate", "Passtime", "Year", "Lat", "Long", "East", "NQA", "x", "PercAvail", "NumAero", "AODMod", "AeroAOD", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "Watercm", "Elev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "CASTNET_12", "CASTNET_18", "Avg_Hema_1")]
colnames(DT13k) <- c("AeroLoc", "JulianDate", "Passtime", "Year", "AeroLat", "AeroLong", "East", "NQA", "x", "PercAvail", "NumAero", "ModAOD", "AeroAOD", "Atype", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter", "AeroWatercm", "AeroElev", "MadElev", "MadSlope", "MedElev", "MedSlope", "NDVI", "PImpervMean", "PImpervSum", "PWetNLCD", "NLCDMode", "Pop", "SO4_Castnet", "Na_Castnet", "Hematite_red")

AllCollocs <- rbind.data.frame(QA3DT, QA2DT, QA1DT, QA3DB, QA2DB, QA1DB, QA3B, QA2B, QA1B, DT33k, DT13k)
AllCollocs$Resolution <- c(rep(10, nrow(QA3DT)), rep(10, nrow(QA2DT)), rep(10, nrow(QA1DT)), rep(10, nrow(QA3DB)), rep(10, nrow(QA2DB)), rep(10, nrow(QA1DB)), rep(10, nrow(QA3B)), rep(10, nrow(QA2B)), rep(10, nrow(QA1B)), rep(3, nrow(DT33k)), rep(3, nrow(DT13k)))
AllCollocs$Algorithm <- c(rep("DT", nrow(QA3DT)), rep("DT", nrow(QA2DT)), rep("DT", nrow(QA1DT)), rep("DB", nrow(QA3DB)), rep("DB", nrow(QA2DB)), rep("DB", nrow(QA1DB)), rep("B", nrow(QA3B)), rep("B", nrow(QA2B)), rep("B", nrow(QA1B)), rep("DT", nrow(DT33k)), rep("DT", nrow(DT13k)))
AllCollocs$QACcode <- c(rep(3, nrow(QA3DT)), rep(2, nrow(QA2DT)), rep(1, nrow(QA1DT)), rep(3, nrow(QA3DB)), rep(2, nrow(QA2DB)), rep(1, nrow(QA1DB)), rep(3, nrow(QA3B)), rep(2, nrow(QA2B)), rep(1, nrow(QA1B)), rep(3, nrow(DT33k)), rep(1, nrow(DT13k)))

rm(DT13k, DT33k, QA1B, QA1DB, QA1DT, QA2B, QA2DB, QA2DT, QA3B, QA3DB, QA3DT)
gc()

# If run when considering only collocations of QAC code of interest
#write.csv(AllCollocs, "H:/Rotation_Yang/AllCollocs.csv")
# If run when considering collocations of greater than or equal to QAC code of interest - QAC 2 removed from this set.
AllCollocsSub <- subset(AllCollocs, AllCollocs$QACcode != 2)
write.csv(AllCollocsSub, "H:/Rotation_Yang/AllCollocs_GECollocs.csv")
rm(AllCollocs, AllCollocsSub)
