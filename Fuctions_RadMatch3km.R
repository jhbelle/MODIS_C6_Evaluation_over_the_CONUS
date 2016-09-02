## ---------------
## Functions - Validation_RadMatch10km.r
## ---------------

## Function 1: GetQAdat
# - Takes as input a data frame consisting of the variables: AeroLoc, JulianDate, Passtime, AODx, AeroType, QAx, and the desired QA code to restrict to.. Then, outputs a data frame containing the variables: AeroLoc, JulianDate, Passtime, AODx, AeroType, NQAx aggregated to the Location, Date, and Time values.
GetQAdat3km <- function(Dat, QAval) {
  QAx <- subset(Dat, Dat[,7]>=QAval)
  NQAx <- aggregate(QAx[,5], list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), length)
  AODMod <- aggregate(QAx[,5], list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  QAx$AeroType[QAx$AeroType == -9999] <- NA
  TypeMode <- t(data.frame(sapply(aggregate(QAx$AeroType, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mfv)[,4], "[[", 1)))
  TypeR1 <- TypeMode[,1]
  SolarZenith <- aggregate(QAx$Solar_Zenith, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  SolarAzimuth <- aggregate(QAx$Solar_Azimuth, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  SensorZenith <- aggregate(QAx$Sensor_Zenith, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  SensorAzimuth <- aggregate(QAx$Sensor_Azimuth, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  Scatter <- aggregate(QAx$Scattering_Angle, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  DatOut <- cbind.data.frame(NQAx, AODMod, TypeR1, SolarZenith, SolarAzimuth, SensorZenith, SensorAzimuth, Scatter)
  colnames(DatOut) <- c("AeroLoc", "JulianDate", "Passtime", "NQA", "AODMod", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter")
  DatOutSub <- subset(DatOut, DatOut$NQA > 2)
  return(DatOutSub)
}

## Function 3: MergeAero
# - Takes as input a QA-code and algorithm-specific dataset containing filtered, valid MODIS retrievals surrounding AERONET stations and merges it with the AERONET values. Then, filters out non-valid collocations.
MergeAero3km <- function(Aero, Modis) {
  Merged <- merge(Modis, Aero, by.x=c("AeroLoc", "JulianDate", "Year"), by.y=c("Location", "Day", "Year"))
  Diff <- abs(Merged$Passtime - Merged$Timestamp)
  SubOuttime <- subset(Merged, Diff <= 30)
  NumObs <- aggregate(SubOuttime$AOD550nm, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), length)
  colnames(NumObs) <- c("Group.1", "Group.2", "Group.3", "Group.4", "NumAero")
  AeroAOD <- aggregate(SubOuttime$AOD550nm, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), mean)[,5]
  Watercm <- aggregate(SubOuttime$Water_cm, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), mean)[,5]
  Lat <- aggregate(SubOuttime$Lat, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), mean)[,5]
  Long <- aggregate(SubOuttime$Long, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), mean)[,5]
  Elev <- aggregate(SubOuttime$Elev, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), mean)[,5]
  AgAero <- cbind.data.frame(NumObs, AeroAOD, Watercm, Lat, Long, Elev)
  NMerge <- merge(Modis, AgAero, by.x=c("AeroLoc", "JulianDate", "Passtime", "Year"), by.y=c("Group.1", "Group.2", "Group.3", "Group.4"))
  NMerge2 <- subset(NMerge, NMerge$NumAero > 1)
  return(NMerge2)
}

## Function 4: Functions to calculate correlations within groups for each dataset
CovStrat3km <- function(dat) {
  return(data.frame(COR = cor(dat$AeroAOD, dat$AODMod)))
}

CorStation3km <- function(QA3DTM) {
  StationCov_QA3DT <- ddply(QA3DTM, .(AeroLoc), CovStrat)
  Lat <- aggregate(QA3DTM$Lat, list(QA3DTM$AeroLoc), mean)
  NumObs <- aggregate(QA3DTM$Long, list(QA3DTM$AeroLoc), length)[,2]
  Long <- aggregate(QA3DTM$Long, list(QA3DTM$AeroLoc), mean)[,2]
  Elev <- aggregate(QA3DTM$Elev, list(QA3DTM$AeroLoc), mean)[,2]
  PercAvail <- aggregate(QA3DTM$PercAvail, list(QA3DTM$AeroLoc), mean)[,2]
  Water <- aggregate(QA3DTM$Watercm, list(QA3DTM$AeroLoc), mean)[,2]
  Byvars <- cbind.data.frame(Lat, Long, Elev, PercAvail, Water, NumObs)
  Tog <- merge(StationCov_QA3DT, Byvars, by.x="AeroLoc", by.y="Group.1")
  return(Tog)
}

CorSeason3km <- function(QA3DTM) {
  QA3DTM$Season <- as.character(cut(QA3DTM$JulianDate, c(0,60,152,244,335,367), labels=c("Winter", "Spring", "Summer", "Fall", "Winter1")))
  QA3DTM$Season[QA3DTM$Season == "Winter1"] <- "Winter"
  StationCov_QA3DT <- ddply(QA3DTM, .(as.factor(Season)), CovStrat)
  return(StationCov_QA3DT)
}

CorMonth3km <- function(QA3DTM) {
  QA3DTM$Month <- as.character(cut(QA3DTM$JulianDate, c(0,31,60,91,121,152,182,213,244,274,305,335,366), labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")))
  StationCov_QA3DT <- ddply(QA3DTM, .(as.factor(Month)), CovStrat)
  return(StationCov_QA3DT)
}
