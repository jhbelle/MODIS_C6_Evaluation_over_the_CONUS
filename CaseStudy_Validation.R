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
# Above file was processed on cluster using

# Get AOD values by product (3 + 1,2,3) for each CMAQ cell and day with a PM value


# Run model for each AOD type, QAC 1+2+3, and QAC 3 only

