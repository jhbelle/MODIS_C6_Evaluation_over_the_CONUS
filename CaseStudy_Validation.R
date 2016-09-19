## -------------------
## Name: CaseStudy_Validation.R
## Program version: R 3.2.3
## Dependencies:
## Author: J.H. Belle
## Purpose: Carry out case study comparing model fits where y=PM2.5 (24 hour) and x=AOD for each AOD product, comparing using QAC 3 only with no filtering and correction versus QAC 1, 2, and 3 with filtering and correction
## -------------------

# Process 24 hour PM2.5 data
AtlDef <- c(-85.6, -83.1, 32.2, 34.5)
# Create empty files to write lines to
write.table(cbind("State", "County", "Site", "ParameterCode", "POC", "Latitude", "Longitude", "Datum", "Date", "24hrPM", "MethodCode"), "H:/Rotation_Yang/CaseStudy/AtlObs24hrFRM.csv", row.names=F, col.names = F, sep=",")

# --------
# Function 1: SortSite - A function to create a list of observations within a study site area
#   NOTE: Study site defined using a vector of coordinates: c(Longitude west/left side, Longitude east/right side, latitude south/bottom side, latitude north/top side)
#   NOTE: Coordinates are assumed to be in the same datum and (optionally) projection as the siteDef coordinates
# --------

SiteSort <- function(dat, siteDef, datLatField="Latitude", datLonField="Longitude"){
  # dat is assumed to be a dataframe, siteDef a vector of coordinates, and datLatField/datLonField are each strings with the field names in dat that correspond to lat/long
  Outp <- subset(dat, dat[datLonField] <= siteDef[2] & dat[datLonField] >= siteDef[1] & dat[datLatField] <= siteDef[4] & dat[datLatField] >= siteDef[3])
  return(Outp)
}

# --------
# Function 5: POCsort - a function designed to take in blocks of observations. If n=1, use value, if n > 1, use value of interest equal to 1 only
# --------
POCsort <- function(dat, varint="POC"){
  if (nrow(dat) > 1){
    dat <- dat[which.min(dat[,varint]),]
  }
  return(dat)
}
library(plyr)

# Loop over years, read in data, pull values of interest, and write those lines to the appropriate text file
for (year in seq(2004,2013)){
  Dat <- read.csv(sprintf("T:/eohprojs/CDC_climatechange/Jess/Dissertation/USEPADailyData/daily_88101_%d.csv", year), stringsAsFactors = F)[,c(1:8,12,15,17,21)]
  print(nrow(Dat))
  Dat <- subset(Dat, Dat$Observation.Count == 1)[,c(1:9,11,12)]
  Atl <- SiteSort(Dat, AtlDef)
  Atl <- unique(Atl)
  Atl <- ddply(Atl, .(State.Code, County.Code, Site.Num, Date.Local), POCsort)
  write.table(Atl, file="H:/Rotation_Yang/CaseStudy/AtlObs24hrFRM.csv", append=T, row.names=F, col.name=F, sep=",")
  rm(Dat)
  gc()
}
rm(list = ls())
gc()

PM24hr <- read.csv("H:/Rotation_Yang/CaseStudy/AtlObs24hrFRM.csv")
# Remove any duplicates
PM24hr <- unique(PM24hr)
# Export list of station locations (lat/long coordinates) in study area
write.csv(unique(PM24hr[,c("State", "County", "Site", "Latitude", "Longitude")]), "H:/Rotation_Yang/CaseStudy/StationLocs.csv")
# Import list of station locations joined to CMAQ grid
PM_CMAQ <- read.csv("H:/Rotation_Yang/CaseStudy/PMCMAQ.csv")[,c(4:6,11)]
# Join back to list of PM observations
PM24hr <- merge(PM24hr, PM_CMAQ)

# Reduce to 1 observation per CMAQ cell per day
Atl24 <- aggregate(X24hrPM ~ Date + Input_FID, PM24hr, mean)

# Tabulate number of distinct CMAQ cells and days in sample
length(unique(Atl24$Input_FID))
length(unique(Atl24$Date))

# Export file with needed AOD values for matching against gridded AOD values on cluster (simple ddply by date, merge on input fid to gridded cluster files (10 km and 3 km) and save to table in same manner as loop above)
write.csv(Atl24, "H:/Rotation_Yang/CaseStudy/PM_DateInputFID.csv")
# Above file was processed on cluster using the script CaseStudy_AggAOD.R on 09/12/2016

# Get AOD values by product (3 + 1,2,3) for each CMAQ cell and day with a PM value
PMAOD <- read.csv("H:/Rotation_Yang/CaseStudy/PM_AOD.csv")
library(nlme)
# Run model for each AOD type, QAC 1+2+3, and QAC 3 only
DT3 <- fitted(lme(X24hrPM ~ DT3, data=PMAOD, random=~DT3|Date, na.action=na.omit))
summary(lm(DT3~na.exclude(PMAOD[,c("Date", "DT3", "X24hrPM")])[,3]))
DTavg1 <- fitted(lme(X24hrPM ~ DTavg1, data=PMAOD, random=~DTavg1|Date, na.action=na.omit))
summary(lm(DTavg1~na.exclude(PMAOD[,c("Date", "DTavg1", "X24hrPM")])[,3]))
#PMAOD <- subset(PMAOD, as.integer(as.character(as.Date(PMAOD$Date, "%Y_%j"), "%m")) < 12 & as.integer(as.character(as.Date(PMAOD$Date, "%Y_%j"), "%m")) > 8)
DTavg2 <- fitted(lme(X24hrPM ~ DTavg2, data=PMAOD, random=~DTavg2|Date, na.action=na.omit))
summary(lm(DTavg2~na.exclude(PMAOD[,c("Date", "DTavg2", "X24hrPM")])[,3]))

DB3 <- fitted(lme(X24hrPM ~ DB3, data=PMAOD, random=~DB3|Date, na.action=na.omit))
summary(lm(DB3~na.exclude(PMAOD[,c("Date", "DB3", "X24hrPM")])[,3]))
DBavg1 <- fitted(lme(X24hrPM ~ DBavg1, data=PMAOD, random=~DBavg1|Date, na.action=na.omit))
summary(lm(DBavg1~na.exclude(PMAOD[,c("Date", "DBavg1", "X24hrPM")])[,3]))
DBavg2 <- fitted(lme(X24hrPM ~ DBavg2, data=PMAOD, random=~DBavg2|Date, na.action=na.omit))
summary(lm(DBavg2~na.exclude(PMAOD[,c("Date", "DBavg2", "X24hrPM")])[,3]))

B3 <- fitted(lme(X24hrPM ~ B3, data=PMAOD, random=~B3|Date, na.action=na.omit))
summary(lm(B3~na.exclude(PMAOD[,c("Date", "B3", "X24hrPM")])[,3]))
Bavg1 <- fitted(lme(X24hrPM ~ Bavg1, data=PMAOD, random=~Bavg1|Date, na.action=na.omit))
summary(lm(Bavg1~na.exclude(PMAOD[,c("Date", "Bavg1", "X24hrPM")])[,3]))
Bavg2 <- fitted(lme(X24hrPM ~ Bavg2, data=PMAOD, random=~Bavg2|Date, na.action=na.omit))
summary(lm(Bavg2~na.exclude(PMAOD[,c("Date", "Bavg2", "X24hrPM")])[,3]))

DT33km <- fitted(lme(X24hrPM ~ DT33km, data=PMAOD, random=~DT33km|Date, na.action=na.omit))
summary(lm(DT33km~na.exclude(PMAOD[,c("Date", "DT33km", "X24hrPM")])[,3]))
DTavg13km <- fitted(lme(X24hrPM ~ DTavg13km, data=PMAOD, random=~DTavg13km|Date, na.action=na.omit))
summary(lm(DTavg13km~na.exclude(PMAOD[,c("Date", "DTavg13km", "X24hrPM")])[,3]))
DTavg23km <- fitted(lme(X24hrPM ~ DTavg23km, data=PMAOD, random=~DTavg23km|Date, na.action=na.omit))
summary(lm(DTavg23km~na.exclude(PMAOD[,c("Date", "DTavg23km", "X24hrPM")])[,3]))

# Need angles; tcpw; ndvi at each CMAQ cell
# For angles can use gridding results to join
# For NDVI and TCPW do direct matching within CMAQ cell boundaries (rough) off of coordinates and take median
# Land use has already been joined to CMAQ, just need to find the layer and add back in

# Get polygon rings - read in pre-preppped polygon file from Arc - only polygons used in analysis selected and included in layer (Arc is better at working with large quantities of polygons)
library(rgdal)
CMAQ_polys <- readOGR("T:/eohprojs/CDC_climatechange/Jess/ValidationPaperRewrite/GISlayers/CaseStudy_CMAQcellsProj.shp", "CaseStudy_CMAQcellsProj")
# This is a clumsy way to do this, but I hate dealing with lapply when the query is this complicated and the coordinates function only extracts midpoints
Lbounds <- rep(NA, 19)
Rbounds <- rep(NA, 19)
Ubounds <- rep(NA, 19)
Lowbounds <- rep(NA, 19)
for (i in seq(1, 19)){
  Lbounds[i] <- min(CMAQ_polys@polygons[[i]]@Polygons[[1]]@coords[,1])
  Rbounds[i] <- max(CMAQ_polys@polygons[[i]]@Polygons[[1]]@coords[,1])
  Lowbounds[i] <- min(CMAQ_polys@polygons[[i]]@Polygons[[1]]@coords[,2])
  Ubounds[i] <- max(CMAQ_polys@polygons[[i]]@Polygons[[1]]@coords[,2])
}
CMAQ_polys@data <- cbind.data.frame(CMAQ_polys@data, Lbounds, Rbounds, Lowbounds, Ubounds)

# Land use
Landuse <- read.csv("H:/Rotation_Yang/CaseStudy/CMAQ_Alldata.csv")
CMAQ_polys@data <- merge(CMAQ_polys@data, Landuse)

# Merge CMAQ polygon definitions back to PMAOD
PMAOD2 <- merge(PMAOD, CMAQ_polys@data, by="Input_FID")

# Write csv
write.csv(PMAOD2, "H:/Rotation_Yang/CaseStudy/PMAODLandPBounds.csv")

# NDVI, QAC codes, TCPW, and angle values were extracted from MODIS files on cluster using the scripts: CaseStudy_ExtractMODIS_Angles.m; CaseStudy_ExtractMODIS_NDVI.R; CaseStudy_ExtractMODIS_TCPW.m; CaseStudy_AggAOD.R
# Read in new AOD file with QAC codes
AODPM <- read.csv("H:/Rotation_Yang/CaseStudy/PM_AOD2.csv")
AODPM$Date <- as.Date(AODPM$Date, "%Y_%j")
# Read in Landcover values
NLCD <- read.csv("H:/Rotation_Yang/CaseStudy/PMAODLandPBounds.csv")[,c(2,4,32)]
NLCD$Date <- as.Date(NLCD$Date, "%Y_%j")
AODPM <- merge(AODPM, NLCD, by=c("Input_FID", "Date"))
# Read in NDVI file
NDVI <- read.csv("H:/Rotation_Yang/CaseStudy/CaseStudy_NDVIvals.csv")
# Scale values and format vars
NDVI$NDVI <- NDVI$NDVI/10000.0
NDVI$Date <- as.Date(sprintf("%d_%03d", NDVI$Year, NDVI$Jday), "%Y_%j")
NDVI <- NDVI[,c(2,5:6)]
# Match each date to the nearest NDVIDate
AODPM$NDVI = rep(NA, length(AODPM$Date))
for (i in 1:length(AODPM$Date)){
  Date = AODPM$Date[i]
  NDVIval = NDVI$NDVI[which.min(abs(NDVI$Date - Date))]
  AODPM$NDVI[i] = NDVIval
}
# Read in TCPW file
TCPW <- read.csv("H:/Rotation_Yang/CaseStudy/MODISExCaseStudyTCPW.csv")
# Scale values and format vars
TCPW$Date <- as.Date(sprintf("%d_%03d", TCPW$Year, TCPW$JulianDate), "%Y_%j")
TCPW <- TCPW[,c(1,4,5)]
TCPW$TCPW <- (TCPW$TCPW*0.0010000000474974513)
# Merge to AODPM
AODPM <- merge(AODPM, TCPW, by=c("Input_FID", "Date"))
# Read in Angles file
Angles <- read.csv("H:/Rotation_Yang/CaseStudy/MODISExCaseStudyAngles.csv")
# Scale values and format vars
Angles$Date <- as.Date(sprintf("%d_%03d", Angles$Year, Angles$JulianDate), "%Y_%j")
Angles[,4:6] <- Angles[,4:6]*0.009999999776482582
Scatter <- aggregate(Scatter ~ Input_FID + Date, Angles, median)
SensorZenith <- aggregate(SensorZenith ~ Input_FID + Date, Angles, median)
SolarZenith <- aggregate(SolarZenith ~ Input_FID + Date, Angles, median)
Angles <- merge(Scatter, SensorZenith)
Angles <- merge(Angles, SolarZenith)

# Merge to AODPM
AODPM <- merge(AODPM, Angles, by=c("Input_FID", "Date"))
# Clean up workspace
rm(NDVI, TCPW, Angles)
gc()

# Read in collocation datafile
AllCollocs <- read.csv("H:/Rotation_Yang/AllCollocs.csv")
AllCollocs <- subset(AllCollocs, AllCollocs$East == 1)
# Filter data
AllCollocs <- subset(AllCollocs, AllCollocs$Scatter <= 165 & AllCollocs$SolarZenith > 15)
# Fit model
# NLCD values aren't all represented in atlanta and most aren't relevant, trimming down to forest, other, developed
AllCollocs$NLCD <- as.factor(ifelse(AllCollocs$NLCDMode > 20 & AllCollocs$NLCDMode < 25, "Dev", ifelse(AllCollocs$NLCDMode > 85 & AllCollocs$NLCDMode < 95, "Wet", "Other")))
AllCollocs$QACcode <- as.factor(AllCollocs$QACcode)
DT <- subset(AllCollocs, AllCollocs$Algorithm == "DT" & AllCollocs$Resolution == 10)
DTM <- lm(AeroAOD ~ ModAOD + NLCD + SensorZenith + NDVI + AeroWatercm + QACcode, DT)
summary(DTM)
DB <- subset(AllCollocs, AllCollocs$Algorithm == "DB" & AllCollocs$Resolution == 10)
DBM <- lm(AeroAOD ~ ModAOD + NLCD + SensorZenith + NDVI + AeroWatercm + QACcode, DB)
summary(DBM)
B <- subset(AllCollocs, AllCollocs$Algorithm == "B" & AllCollocs$Resolution == 10)
BM <- lm(AeroAOD ~ ModAOD + NLCD + SensorZenith + NDVI + AeroWatercm + QACcode, B)
summary(BM)
DT3 <- subset(AllCollocs, AllCollocs$Algorithm == "DT" & AllCollocs$Resolution == 3)
#DT3 <- subset(AllCollocs, AllCollocs$AeroLoc == "Walker_Branch" | AllCollocs$AeroLoc == "Georgia_Tech" | AllCollocs$AeroLoc == "SEARCH-Yorkville")
DT3M <- lm(AeroAOD ~ ModAOD, DT3)
summary(DT3M)
rm(B, DB, DT, DT3)

# Apply filter to case study data
AODPM2 <- subset(AODPM, AODPM$Scatter <= 165 & AODPM$SolarZenith > 15)
#AODPM2 <- AODPM
# Calculate percentage of observations lost to filtering
1-nrow(AODPM2)/nrow(AODPM)
# Reformat landcover
AODPM2$NLCD <- as.factor(ifelse(AODPM2$Landcover > 20 & AODPM2$Landcover < 25, "Dev", ifelse(AODPM2$Landcover > 85 & AODPM2$Landcover < 95, "Wet", "Other")))

library(nlme)

# 10km DT
DT <- AODPM2[,c("Input_FID", "Date", "DTavg2", "NLCD", "SensorZenith", "NDVI", "TCPW", "DTavg1", "X24hrPM")]
# Reformat QACcode
DT$DTavg1 <- as.factor(DT$DTavg1)
# Rename columns
colnames(DT) <- c("Input_FID", "Date", "ModAOD", "NLCD", "SensorZenith", "NDVI", "AeroWatercm", "QACcode", "PM")
DT <- subset(DT, as.integer(as.character(DT$Date, "%m")) < 12 & as.integer(as.character(DT$Date, "%m")) > 8)
# Apply correction to case study data
DT$FiltCorAOD <- predict(DTM, DT)
# Fit lme
DTlme <- fitted(lme(PM ~ FiltCorAOD, data=DT, random=~FiltCorAOD|Date, na.action=na.omit))
summary(lm(DTlme~na.exclude(DT[,c("Date", "FiltCorAOD", "PM")])[,3]))

# 10km DB
DB <- AODPM2[,c("Input_FID", "Date", "DBavg2", "NLCD", "SensorZenith", "NDVI", "TCPW", "DBavg1", "X24hrPM")]
# Reformat QACcode
DB$DBavg1 <- as.factor(DB$DBavg1)
# Rename columns
colnames(DB) <- c("Input_FID", "Date", "ModAOD", "NLCD", "SensorZenith", "NDVI", "AeroWatercm", "QACcode", "PM")
# Apply correction to case study data
DB$FiltCorAOD <- predict(DBM, DB)
# Fit lme
DBlme <- fitted(lme(PM ~ FiltCorAOD, data=DB, random=~FiltCorAOD|Date, na.action=na.omit))
summary(lm(DBlme~na.exclude(DB[,c("Date", "FiltCorAOD", "PM")])[,3]))

# 10km B
B <- AODPM2[,c("Input_FID", "Date", "Bavg2", "NLCD", "SensorZenith", "NDVI", "TCPW", "Bavg1", "X24hrPM")]
# Reformat QACcode
B$Bavg1 <- as.factor(B$Bavg1)
# Rename columns
colnames(B) <- c("Input_FID", "Date", "ModAOD", "NLCD", "SensorZenith", "NDVI", "AeroWatercm", "QACcode", "PM")
# Apply correction to case study data
B$FiltCorAOD <- predict(BM, B)
# Fit lme
Blme <- fitted(lme(PM ~ FiltCorAOD, data=B, random=~FiltCorAOD|Date, na.action=na.omit))
summary(lm(Blme~na.exclude(B[,c("Date", "FiltCorAOD", "PM")])[,3]))

# 3km DT
DT3 <- AODPM2[,c("Input_FID", "Date", "DTavg23km", "NLCD", "SensorZenith", "Scatter", "SolarZenith", "NDVI", "TCPW", "DTavg13km", "X24hrPM")]
# Reformat QACcode
DT3$DTavg13km <- as.factor(DT3$DTavg13km)
# Rename columns
colnames(DT3) <- c("Input_FID", "Date", "ModAOD", "NLCD", "SensorZenith", "Scatter", "SolarZenith", "NDVI", "AeroWatercm", "QACcode", "PM")
# Apply correction to case study data
DT3$FiltCorAOD <- predict(DT3M, DT3)
# Fit lme
DT3lme <- fitted(lme(PM ~ FiltCorAOD, data=DT3, random=~FiltCorAOD|Date, na.action=na.omit))
summary(lm(DT3lme~na.exclude(DT3[,c("Date", "FiltCorAOD", "PM")])[,3]))

DTavg23km <- fitted(lme(PM ~ ModAOD + NLCD + AeroWatercm + NDVI, data=DT3, random=~ModAOD|Date, na.action=na.omit))
summary(lm(DTavg23km~na.exclude(DT3[,c("Date", "ModAOD", "PM")])[,3]))
