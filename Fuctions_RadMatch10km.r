## ---------------
## Functions - Validation_RadMatch10km.r
## ---------------

## Function 1: GetQAdat
# - Takes as input a data frame consisting of the variables: AeroLoc, JulianDate, Passtime, AODx, AeroType, QAx, and the desired QA code to restrict to.. Then, outputs a data frame containing the variables: AeroLoc, JulianDate, Passtime, AODx, AeroType, NQAx aggregated to the Location, Date, and Time values.
GetQAdat <- function(Dat, QAval) {
  require(modeest)
  # Subset data to QAC value of interest
  QAx <- subset(Dat, Dat[,6]>=QAval)
  # Calculate statistics for each unique combination of location, date, and time
  NQAx <- aggregate(QAx[,4], list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), length)
  AODavg <- aggregate(QAx[,4], list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  SolarZenith <- aggregate(QAx$SolarZenith, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  SolarAzimuth <- aggregate(QAx$SolarAzimuth, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  SensorZenith <- aggregate(QAx$SensorZenith, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  SensorAzimuth <- aggregate(QAx$SensorAzimuth, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  Scatter <- aggregate(QAx$Scatter, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mean)[,4]
  QAx$AeroType[QAx$AeroType == -9999] <- NA
  TypeMode <- t(data.frame(sapply(aggregate(QAx$AeroType, list(QAx$AeroLoc, QAx$JulianDate, QAx$Passtime), mfv)[,4], "[[", 1)))
  TypeR1 <- TypeMode[,1]
  # Create output dataset
  DatOut <- cbind.data.frame(NQAx, AODavg, TypeR1, SolarZenith, SolarAzimuth, SensorZenith, SensorAzimuth, Scatter)
  colnames(DatOut) <- c("AeroLoc", "JulianDate", "Passtime", "NQA", "ModAod", "Atype1", "SolarZenith", "SolarAzimuth", "SensorZenith", "SensorAzimuth", "Scatter")
  # Remove collocations with fewer than 2 pixels included in the average
  DatOutSub <- subset(DatOut, DatOut$NQA > 2)
  return(DatOutSub)
}

## Function 2: StackYrsDat
# - Takes as input a dataset for each year, the QA code of interest, and the columns of interest for that retrieval algorithm and QA code processes each year of data and stacks the answers on top of each other
StacksYrsDat <- function(Dat04, Dat05, Dat06, Dat07, Dat08, Dat09, Dat10, Dat11, Dat12, Dat13, QAval, Cols) {
  D04 <- Dat04[,Cols]
  NumObs04 <- aggregate(D04[,4], list(D04$AeroLoc, D04$JulianDate, D04$Passtime), length)
  D04Ag <- merge(GetQAdat(D04, QAval), NumObs04, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3"))
  D05 <- Dat05[,Cols]
  NumObs05 <- aggregate(D05[,4], list(D05$AeroLoc, D05$JulianDate, D05$Passtime), length)
  D05Ag <- merge(GetQAdat(D05, QAval), NumObs05, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3"))
  D06 <- Dat06[,Cols]
  NumObs06 <- aggregate(D06[,4], list(D06$AeroLoc, D06$JulianDate, D06$Passtime), length)
  D06Ag <- merge(GetQAdat(D06, QAval), NumObs06, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3"))
  D07 <- Dat07[,Cols]
  NumObs07 <- aggregate(D07[,4], list(D07$AeroLoc, D07$JulianDate, D07$Passtime), length)
  D07Ag <- merge(GetQAdat(D07, QAval), NumObs07, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3"))
  D08 <- Dat08[,Cols]
  NumObs08 <- aggregate(D08[,4], list(D08$AeroLoc, D08$JulianDate, D08$Passtime), length)
  D08Ag <- merge(GetQAdat(D08, QAval), NumObs08, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3"))
  D09 <- Dat09[,Cols]
  NumObs09 <- aggregate(D09[,4], list(D09$AeroLoc, D09$JulianDate, D09$Passtime), length)
  D09Ag <- merge(GetQAdat(D09, QAval), NumObs09, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3"))
  D10 <- Dat10[,Cols]
  NumObs10 <- aggregate(D10[,4], list(D10$AeroLoc, D10$JulianDate, D10$Passtime), length)
  D10Ag <- merge(GetQAdat(D10, QAval), NumObs10, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3"))
  D11 <- Dat11[,Cols]
  NumObs11 <- aggregate(D11[,4], list(D11$AeroLoc, D11$JulianDate, D11$Passtime), length)
  D11Ag <- merge(GetQAdat(D11, QAval), NumObs11, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3"))
  D12 <- Dat12[,Cols]
  NumObs12 <- aggregate(D12[,4], list(D12$AeroLoc, D12$JulianDate, D12$Passtime), length)
  D12Ag <- merge(GetQAdat(D12, QAval), NumObs12, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3"))
  D13 <- Dat13[,Cols]
  NumObs13 <- aggregate(D13[,4], list(D13$AeroLoc, D13$JulianDate, D13$Passtime), length)
  D13Ag <- merge(GetQAdat(D13, QAval), NumObs13, by.x=c("AeroLoc", "JulianDate", "Passtime"), by.y=c("Group.1", "Group.2", "Group.3"))
  Outp <- rbind.data.frame(D04Ag, D05Ag, D06Ag, D07Ag, D08Ag, D09Ag, D10Ag, D11Ag, D12Ag, D13Ag)
  Outp$PercAvail <- (Outp$NQA/Outp$x)*100
  Outp$Year <- c(rep(2004, length(D04Ag[,1])), rep(2005, length(D05Ag[,1])), rep(2006, length(D06Ag[,1])), rep(2007, length(D07Ag[,1])), rep(2008, length(D08Ag[,1])), rep(2009, length(D09Ag[,1])), rep(2010, length(D10Ag[,1])), rep(2011, length(D11Ag[,1])), rep(2012, length(D12Ag[,1])), rep(2013, length(D13Ag[,1])))
  OutpSub <- subset(Outp, Outp$PercAvail >= 20)
  return(OutpSub)
}

## Function 3: MergeAero
# - Takes as input a QA-code and algorithm-specific dataset containing filtered, valid MODIS retrievals surrounding AERONET stations and merges it with the AERONET values. Then, filters out non-valid collocations.
MergeAero <- function(Aero, Modis) {
  Merged <- merge(Modis, Aero, by.x=c("AeroLoc", "JulianDate", "Year"), by.y=c("Location", "Day", "Year"))
  Diff <- abs(Merged$Passtime - Merged$Timestamp)
  SubOuttime <- subset(Merged, Diff <= 30)

  NumObs <- aggregate(SubOuttime$AOD550nm, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), length)
  colnames(NumObs) <- c("Group.1", "Group.2", "Group.3", "Group.4", "NumAero")
  MeanAOD <- aggregate(SubOuttime$AOD550nm, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), mean)[,5]
  Watercm <- aggregate(SubOuttime$Water_cm, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), mean)[,5]
  Lat <- aggregate(SubOuttime$Lat, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), mean)[,5]
  Long <- aggregate(SubOuttime$Long, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), mean)[,5]
  Elev <- aggregate(SubOuttime$Elev, list(SubOuttime$AeroLoc, SubOuttime$JulianDate, SubOuttime$Passtime, SubOuttime$Year), mean)[,5]
  AgAero <- cbind.data.frame(NumObs, MeanAOD, Watercm, Lat, Long, Elev)
  NMerge <- merge(Modis, AgAero, by.x=c("AeroLoc", "JulianDate", "Passtime", "Year"), by.y=c("Group.1", "Group.2", "Group.3", "Group.4"))
  NMerge2 <- subset(NMerge, NMerge$NumAero > 1)
  return(NMerge2)
}

## Function 4-7: Functions to calculate correlations within groups for each dataset
CovStrat <- function(dat) {
  return(data.frame(COR = cor(dat$MeanAOD, dat$ModAod)))
}

CorStation <- function(QA3DTM) {
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

CorSeason <- function(QA3DTM) {
  QA3DTM$Season <- as.character(cut(QA3DTM$JulianDate, c(0,60,152,244,335,367), labels=c("Winter", "Spring", "Summer", "Fall", "Winter1")))
  QA3DTM$Season[QA3DTM$Season == "Winter1"] <- "Winter"
  StationCov_QA3DT <- ddply(QA3DTM, .(as.factor(Season)), CovStrat)
  return(StationCov_QA3DT)
}

CorMonth<- function(QA3DTM) {
  QA3DTM$Month <- as.character(cut(QA3DTM$JulianDate, c(0,31,60,91,121,152,182,213,244,274,305,335,366), labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")))
  StationCov_QA3DT <- ddply(QA3DTM, .(as.factor(Month)), CovStrat)
  return(StationCov_QA3DT)
}

## Function 8: Extreme values analysis function - note: subset should be a vector of the same length as and will be coerced to factor
ExVals <- function(data, xvar, yvar, thresh=0.2, subseter=NULL){
  if (is.null(subseter)){
    x <- data[,c(xvar)]
    y <- data[,c(yvar)]
    xGr1 = ifelse(x > thresh, 1, 0)
    yGr1 = ifelse(y > thresh, 1, 0)
    Nx <- sum(xGr1)
    Ny <- sum(yGr1)
    N = length(x)
    Se <- sum(yGr1[which(xGr1 == 1)])/Nx
    Sp <- 1 - (sum(yGr1[which(xGr1 != 1)])/(N-Nx))
    Out <- cbind.data.frame(thresh, N, Nx, Ny, Se, Sp)
  } else {
    Out = matrix(NA, nrow=nlevels(subseter), ncol=7)
    for (i in 1:nlevels(subseter)){
      xGr1 = ifelse(x > thresh[i] & subseter == levels(subseter)[i], 1, 0)
      yGr1 = ifelse(y > thresh[i] & subseter == levels(subseter)[i], 1, 0)
      Nx <- sum(xGr1)
      Ny <- sum(yGr1)
      N = length(subset(x, subseter==levels(subseter)[i]))
      Se <- sum(yGr1[which(xGr1 == 1)])/Nx
      Sp <- 1 - (sum(yGr1[which(xGr1 != 1 & subseter==levels(subseter)[i])])/(N-Nx))
      Out[i,] <- c(thresh[i], levels(subseter)[i], N, Nx, Ny, Se, Sp)
    }
    Out = as.data.frame(Out)
    colnames(Out) <- c("Thresh", "Cat", "N", "Nx", "Ny", "Se", "Sp")
  }
  return(Out)
}

## Function 9: AddEW - adds regional designation into collocation dataset, given regional designations
AddEW <- function(East, QA3DTM){
  QA3DTM <- merge(QA3DTM, East, by.x="AeroLoc", by.y="East_locs", all.x=T)
  QA3DTM$East <- ifelse(is.na(QA3DTM$East), 0,1)
  QA3DTM$East <- cut(QA3DTM$East, breaks=c(-0.5, 0.5, 1.5), labels=c("West", "East"))
  return(QA3DTM)
}

## Function 10: AddAncillary - adds all additional information on collocations - slope, elevation, ndvi, land use, population, percent impervious, percent water, east/west, castnet and hematite concentrations
AddAncillary <- function(directory, Collocs){
  filelist <- list.files(directory, full.names=T)
  print(length(filelist))
  for (file in filelist){
    fileR <- read.csv(file)[,2:6]
    Collocs <- merge(Collocs, fileR, by=c("AeroLoc", "JulianDate", "Passtime", "Year"), all.x=T)
  }
  newnames <- unlist(strsplit(list.files(directory), "[.]"))
  colnames(Collocs) <- c(colnames(Collocs[1:20]), newnames[which(newnames!="csv")])
  East_locs <- read.csv("H:/Rotation_Yang/EastWestSplit/EastLocs100Long.csv")[,3]
  East <- rep(1, length(East_locs))
  East <- cbind.data.frame(East_locs, East)
  Collocs <- merge(Collocs, East, by.x="AeroLoc", by.y="East_locs", all.x=T)
  Collocs$East <- ifelse(is.na(Collocs$East), 0,1)
  CASTNET <- read.csv("H:/Rotation_Yang/HygroscopicAnal/Aeronet_castnet_approx.csv")[,c("Location", "CASTNET_12", "CASTNET_18")]
  Collocs <- merge(Collocs, CASTNET, by.x="AeroLoc", by.y="Location", all.x=T)
  Red <- read.csv("H:/Rotation_Yang/Iron_red/Hematite_50kmBuffer.csv")[,c("Location", "Avg_Hema_1")]
  Collocs <- merge(Collocs, Red, by.x="AeroLoc", by.y="Location", all.x=T)
  return(Collocs)
}

## Function 11: RunLMDD - function to run a linear model and returns the intercept and slope values and ajusted r^2 as a data frame
RunLMDD <- function(data, form){
  Model <- summary(lm(form, data))
  Out <- cbind.data.frame(Model$coefficients[1,1], Model$coefficients[2,1], Model$r.squared)
  colnames(Out) <- c("a", "b", "adjR2")
  return(Out)
}

## Function 12: PercEE - function to determine the percentage of observations within, above, and below the EE for MODIS AOD. Variable names for Modis (Mod) and Aeronet (Aero) AOD must be passed in as strings
PercEE <- function(Data, Mod, Aero){
  eeup = Data[,Aero] + (0.05+(0.15*Data[,Aero]))
  eed = Data[,Aero] - (0.05+(0.15*Data[,Aero]))
  AboveEE = ifelse(Data[,Mod] > eeup, 1, 0)
  BelowEE = ifelse(Data[,Mod] < eed, 1, 0)
  InEE = ifelse(AboveEE == 0 & BelowEE== 0, 1, 0)
  Out <- cbind.data.frame(sum(AboveEE)/length(AboveEE), sum(BelowEE)/length(BelowEE), sum(InEE)/length(InEE))
  colnames(Out) <- c("PAboveEE", "PBelowEE", "PInEE")
  return(Out)
}

## Function 13: PullStats - function to pull statistics for each variable of interest from a dataset and return a dataframe with a row for each variable of interest. varMed must be in quotes
PullMed <- function(Data, OfInterest, varMed, statInt="median"){
  meddat <- NULL
  for (i in 1:length(OfInterest)){
    meds <- aggregate(Data[,varMed], by=list(Data[,OfInterest[i]]), FUN=statInt)
    meds <- cbind.data.frame(meds, rep(OfInterest[i], nrow(meds)))
    colnames(meds) <- c("OfIntVal", "MedianErr", "OfIntVar")
    meddat <- rbind.data.frame(meddat, meds)
  }
  return(meddat)
}

## Function 14: g_legend - function to get legend from a ggplot - originally posted on: http://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}


## Function 15: Spline knot analysis - function to run spline model for a given knot, and pull out AIC values
Splknot <- function(dat, knot, Splinevar="ModAOD", eqn=AeroAOD~ModAOD + spl){
  dat$spl <- ifelse(dat[,Splinevar] > knot, dat[,Splinevar] - knot, 0)
  lmOut <- summary(lm(eqn, data=dat))$adj.r.squared
  #lmOut <- AIC(lm(eqn, data=dat))
  return(lmOut)
}

# Function 16: Calculates percent coverage and population-weighted percent coverage statistics in coverage analysis
CalcRes2 <- function(x){
  Covs <- 100*c(mean(x$PCovDT3), mean(x$PCovDT123), mean(x$PCovDB3), mean(x$PCovDB123), mean(x$PCovB3), mean(x$PCovB123), mean(x$PCov3kmDT3), mean(x$PCov3kmDT123))
  PCovs <- 100*(c(sum(x$PCovDT3*x$Pop), sum(x$PCovDT123*x$Pop), sum(x$PCovDB3*x$Pop), sum(x$PCovDB123*x$Pop), sum(x$PCovB3*x$Pop), sum(x$PCovB123*x$Pop), sum(x$PCov3kmDT3*x$Pop), sum(x$PCov3kmDT123*x$Pop)))/sum(x$Pop)
  QAC <- rep(c("3", "123"), 4)
  Alg <- c(rep("10 km DT", 2), rep("10 km DB", 2), rep("10 km DB-DT", 2), rep("3 km DT", 2))
  Out <- cbind.data.frame(Alg, QAC, Covs, PCovs)
  QAC3 <- subset(Out[c(1,3,4)], QAC=="3")
  QAC123 <- subset(Out[,c(1,3,4)], QAC=="123")
  Out <- merge(QAC3, QAC123, by="Alg")
  colnames(Out) <- c("Alg", "Covs3", "PCovs3", "Covs123", "PCovs123")
  return(Out)
}
