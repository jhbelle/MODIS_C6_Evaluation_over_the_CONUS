## --------------
## Name: CaseStudy_AggAOD.R
## Program version: R 3.3.0
## Dependencies: plyr
## Author: J.H. Belle
## Purpose: For each day, merge the files containing the AOD values (QA 3, 1+2+3) for each of the 4 products to the input FID values from the CMAQ grid where PM_2.5 observations are present. Return the same table as input, but with a set of 8 AOD values accompanying each PM_2.5 value.
## --------------

## Read in the data file
Dat <- read.csv("/aqua/Jess/Data/PM_DateInputFID.csv")[,c(2:4)]
# Reformat date variable as year and julian date
Dat$Date <- as.character(as.Date(Dat$Date, "%Y-%m-%d"), "%Y_%j")

## Define locations of AOD values
AOD10kmLoc <- "/terra/MODIS_Jess/Daily_gridding_output/DailyGridAOD_" # File names follow DailyGridAOD_%Y_%03d.csv; US.id is input FID values; Need to take DT3, average of DT3, DT2, and DT1, best value of DT3, DT2 and DT1 (i.e. if exists(DT3) take DT3, else if exists(DT2) take DT2, else if exists(DT1) take DT1). Repeat for DB1, DB2, DB3 and B1, B2, and B3.
AOD3kmLoc <- "/terra/MODIS_Jess/Daily_gridding3km/DailyGridAOD_" # File names follow DailyGridAOD_%Y_%03d.csv; US.id is input FID values; Need to take DT3, average of DT3, DT2, and DT1, best value of DT3, DT2 and DT1 (i.e. if exists(DT3) take DT3, else if exists(DT2) take DT2, else if exists(DT1) take DT1).

## Define function for each day
DayAgg <- function(dat, LocationDat1=AOD10kmLoc, LocationDat2=AOD3kmLoc){
  AOD10km <- try(read.csv(paste(LocationDat1, dat$Date[1], ".csv", sep="")))
  if (is.data.frame(AOD10km)) {
    AOD10km$DTavg1 <-  ifelse(!is.na(AOD10km$DT3), 3, ifelse(!is.na(AOD10km$DT2), 2, 1))
    AOD10km$DBavg1 <- ifelse(!is.na(AOD10km$DB3), 3, ifelse(!is.na(AOD10km$DB2), 2, 1))
    AOD10km$Bavg1 <-  ifelse(!is.na(AOD10km$B3), 3, ifelse(!is.na(AOD10km$B2), 2, 1))
    AOD10km$DTavg2 <- ifelse(!is.na(AOD10km$DT3), AOD10km$DT3, ifelse(!is.na(AOD10km$DT2), AOD10km$DT2, AOD10km$DT1))
    AOD10km$DBavg2 <- ifelse(!is.na(AOD10km$DB3), AOD10km$DB3, ifelse(!is.na(AOD10km$DB2), AOD10km$DB2, AOD10km$DB1))
    AOD10km$Bavg2 <- ifelse(!is.na(AOD10km$B3), AOD10km$B3, ifelse(!is.na(AOD10km$B2), AOD10km$B2, AOD10km$B1))
    AOD10km <- AOD10km[,c("US.id", "DT3", "DTavg1", "DTavg2", "DB3", "DBavg1", "DBavg2", "B3", "Bavg1", "Bavg2")]
    Dat_AOD10km <- merge(dat, AOD10km, by.x=c("Input_FID"), by.y=c("US.id"))
  } else { 
    Dat_AOD10km = dat
    Dat_AOD10km$DT3 = NA
    Dat_AOD10km$DTavg1 = NA
    Dat_AOD10km$DTavg2 = NA
    Dat_AOD10km$DB3 = NA
    Dat_AOD10km$DBavg1 = NA
    Dat_AOD10km$DBavg2 = NA
    Dat_AOD10km$B3 = NA
    Dat_AOD10km$Bavg1 = NA
    Dat_AOD10km$Bavg2 = NA
  }
  AOD3km <- try(read.csv(paste(LocationDat2, dat$Date[1], ".csv", sep=""))[,2:5])
  if (is.data.frame(AOD3km)) {
    AOD3km$DTavg13km <- rowMeans(AOD3km[,c("DT3", "DT2", "DT1")], na.rm=T)
    AOD3km$DT33km <- AOD3km$DT3
    AOD3km$DTavg23km <- ifelse(!is.na(AOD3km$DT3), AOD3km$DT3, ifelse(!is.na(AOD3km$DT2), AOD3km$DT2, AOD3km$DT1))
    AOD3km <- AOD3km[,c("US.id", "DT33km", "DTavg13km", "DTavg23km")]
    Outp <- merge(Dat_AOD10km, AOD3km, by.x="Input_FID", by.y="US.id")
  } else {
    Outp = Dat_AOD10km
    Outp$DT33km = NA
    Outp$DTavg13km = NA
    Outp$DTavg23km = NA
  }
  return(Outp)
}

## Open plyr library
library(plyr)

## Run function over each day
PM_AOD <- ddply(Dat, .(Date), DayAgg)

# Save results
write.csv(PM_AOD, "/aqua/Jess/Data/PM_AOD2.csv")
